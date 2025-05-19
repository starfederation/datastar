using StarFederation.Datastar.Enumerations;
using StarFederation.Datastar.Interfaces;

namespace StarFederation.Datastar.Types.Options;

/// <summary>
///     Options for merging fragments in server-sent events.
/// </summary>
public class ServerSentEventMergeFragmentsOptions : MergeFragmentsOptions, IDatastarServerSentEventOptions
{
}