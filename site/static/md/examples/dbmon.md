## DBmon

## Explanation

Per a conversation on the discord server there was a desire to port an old React Conf talk, [DBMon](https://conf2015.reactjs.org/schedule.html#hype), to Datastar.

The logic is 1:1 but all done on the backend, and since it's Go, it's an interesting comparison to the SPA based approach.  We've limited purely since the site is run on a free tier server and don't want to be a bad user.  If you run the site from source you can easily 10x the rows without major issues (though we don't think this is the best approach).  If you are doing larger datasets you should use Datastar's ability to update signals without the need to generate html fragments.

Also of note if you open your Network tab in DevTools we are leveraging ZSTD compression so the data rate is relatively low for the contents.

## Demo

<div
    id="contents"
    data-on-load="@get('/examples/dbmon/contents')"
>
</div>
