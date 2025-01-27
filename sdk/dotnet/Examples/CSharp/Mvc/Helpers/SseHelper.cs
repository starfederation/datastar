using System.Text;

namespace Mvc.Helpers;

public static class SseHelper
{
    public static async Task SetSseHeaders(HttpResponse response)
    {
        response.Headers.Append("Cache-Control", "no-cache");
        response.Headers.Append("Content-Type", "text/event-stream");
        response.Headers.Append("Connection", "keep-alive");
        await response.Body.FlushAsync();
    }

    public static async Task SendServerSentEvent(
        HttpResponse response
        , string fragment
        , string selector = null
        , string mergeMode = null
        , int settleDuration = 300
        , bool useViewTransition = false
        , bool end = false)
    {
        // Clean the fragment by removing all newlines and extra spaces
        fragment = fragment
            .Replace(Environment.NewLine, "")
            .Replace("\n", "")
            .Replace("\r", "")
            .Trim();

        var data = "event: datastar-merge-fragments\n";

        if (!string.IsNullOrEmpty(selector)) data += $"data: selector {selector}\n";

        if (!string.IsNullOrEmpty(mergeMode)) data += $"data: merge {mergeMode}\n";

        if (settleDuration != 300) data += $"data: settle {settleDuration}\n";

        if (useViewTransition) data += "data: view-transition\n";

        data += $"data: fragments {fragment}\n\n";

        await response.Body.WriteAsync(Encoding.UTF8.GetBytes(data));
        await response.Body.FlushAsync();

        if (end) response.Body.Close();
    }
}