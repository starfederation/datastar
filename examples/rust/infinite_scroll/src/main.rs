use {
    axum::{
        Router,
        response::{Html, Response},
        routing::get,
    },
    datastar::prelude::{IntoResponse, MergeFragments, MergeMode, ReadSignals},
    rinja::Template,
    serde::{Deserialize, Serialize},
    std::error::Error,
    tokio::net::TcpListener,
};

#[derive(Template)]
#[template(path = "index.html")]
pub struct IndexTemplate {
    pub agents: AgentsPartial,
}

#[derive(Template)]
#[template(path = "agents.html")]
pub struct AgentsPartial {
    pub agents: Vec<AgentPartial>,
    pub more: MorePartial,
    pub signals: Signals,
}

#[derive(Template)]
#[template(path = "agent.html")]
pub struct AgentPartial {
    pub i: u32,
}

#[derive(Template)]
#[template(path = "more.html")]
pub struct MorePartial {
    pub offset: u32,
    pub limit: u32,
}

#[derive(Template)]
#[template(path = "rickroll.html")]
pub struct RickRollPartial;

#[derive(Debug, Serialize, Deserialize)]
pub struct Signals {
    pub limit: u32,
    pub offset: u32,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let listener = TcpListener::bind("localhost:3000").await?;

    let app = Router::new()
        .route("/", get(index))
        .route("/infinite_scroll", get(infinite_scroll));

    axum::serve(listener, app).await?;

    Ok(())
}

pub async fn index() -> Html<String> {
    Html(
        IndexTemplate {
            agents: AgentsPartial {
                agents: vec![],
                more: MorePartial {
                    offset: 0,
                    limit: 10,
                },
                signals: Signals {
                    offset: 0,
                    limit: 10,
                },
            },
        }
        .render()
        .unwrap(),
    )
}

pub async fn infinite_scroll(ReadSignals(mut signals): ReadSignals<Signals>) -> Response {
    signals.limit = signals.limit.clamp(1, 100);

    match signals.offset {
        0 => {
            let agents = (0..signals.limit).map(|i| AgentPartial { i }).collect();

            let partial = AgentsPartial {
                agents,
                more: MorePartial {
                    offset: signals.offset + signals.limit,
                    limit: signals.limit,
                },
                signals,
            };

            MergeFragments::new(partial.render().unwrap()).into_response()
        }
        offset if offset < 100 => {
            let more = MergeFragments::new(
                MorePartial {
                    offset: offset + signals.limit,
                    limit: signals.limit,
                }
                .render()
                .unwrap(),
            );

            let agents = (0..signals.limit)
                .map(|i| {
                    MergeFragments::new(AgentPartial { i: offset + i }.render().unwrap())
                        .selector("#click_to_load_rows")
                        .merge_mode(MergeMode::Append)
                })
                .collect::<Vec<_>>();

            (more, agents).into_response()
        }
        _ => MergeFragments::new(RickRollPartial.render().unwrap()).into_response(),
    }
}
