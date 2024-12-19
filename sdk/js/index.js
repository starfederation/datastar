/**
 * ServerSentEventGenerator is responsible for initializing and handling
 * server-sent events (SSE) for different web frameworks.
 */
export const ServerSentEventGenerator = {
  /**
   * Initializes the server-sent event generator.
   *
   * @param {object} res - The response object from the framework.
   * @returns {Object} Methods for manipulating server-sent events.
   */
  init: function (req, res) {
    return {
      req: req,
      res: res,
      /**
       * @typedef {Object} SendOptions
       * @property {?string} [eventId=null] - Event ID to attach.
       * @property {number} [retryDuration=1000] - Retry duration in milliseconds.
       */

      /**
       * Sends a server-sent event (SSE) to the client.
       *
       * @param {string} eventType - The type of the event.
       * @param {string[]} dataLines - Lines of data to send.
       * @param {SendOptions} [sendOptions] - Additional options for sending events.
       */
      _send: function (
        eventType,
        dataLines,
        sendOptions = {
          eventId: null,
          retryDuration: 1000,
        }
      ) {
        let data = dataLines.map((line) => `data: ${line}\n`).join("") + "\n";

        this.res.setHeader("Cache-Control", "nocache");
        this.res.setHeader("Connection", "keep-alive");
        this.res.setHeader("Content-Type", "text/event-stream");

        if (sendOptions.eventId != null) {
          this.res.write(`id: ${sendOptions.eventId}\n`);
        }
        if (eventType) {
          this.res.write(`event: ${eventType}\n`);
        }
        this.res.write(`retry: ${sendOptions.retryDuration}\n`);
        this.res.write(data);
      },
      /**
       * Reads signals based on HTTP methods and merges them with provided signals.
       *
       * @param {object} signals - Predefined signals to merge with.
       * @returns {Promise<object>} Merged signals object.
       */
      ReadSignals: async function (signals) {
        if (this.req.method === "GET") {
          return {
            ...signals,
            ...JSON.parse(this.req.query_parameters.datastar),
          };
        } else {
          const body = await this.req.json();
          return { ...signals, ...body };
        }
      },
      /**
       * @typedef {Object} MergeFragmentsOptions
       * @property {string} [selector] - CSS selector affected.
       * @property {string} [mergeMode="morph"] - Mode for merging.
       * @property {number} [settleDuration=300] - Duration to settle.
       * @property {?boolean} [useViewTransition=null] - Use view transition.
       * @property {?string} [eventId=null] - Event ID to attach.
       * @property {?number} [retryDuration=null] - Retry duration in milliseconds.
       */

      /**
       * Sends a merge fragments event.
       *
       * @param {string[]} fragments - Array of fragment identifiers.
       * @param {MergeFragmentsOptions} options - Additional options for merging.
       * @throws Will throw an error if fragments are missing.
       */
      MergeFragments: function (
        fragments,
        options = {
          selector: null,
          mergeMode: "morph",
          settleDuration: 300,
          useViewTransition: null,
          eventId: null,
          retryDuration: null,
        }
      ) {
        let dataLines = [];
        if (options?.selector != null)
          dataLines.push(`selector ${options.selector}`);
        if (options?.settleDuration != null)
          dataLines.push(`settleDuration ${options.settleDuration}`);
        if (options?.useViewTransition != null)
          dataLines.push(`useViewTransition ${options.useViewTransition}`);
        if (fragments) {
          fragments.map((frag) => {
            dataLines.push(`fragments ${frag.replace(/[\r\n]+/g, "")}`);
          });
        } else {
          throw Error("MergeFragments missing fragement(s).");
        }
        return this._send("datastar-merge-fragments", dataLines, {
          eventId: options.eventId,
          retryDuration: options.retryDuration,
        });
      },
      /**
       * @typedef {Object} RemoveFragmentsOptions
       * @property {number} [settleDuration] - Duration to settle.
       * @property {?boolean} [useViewTransition=null] - Use view transition.
       * @property {?string} [eventId=null] - Event ID to attach.
       * @property {?number} [retryDuration=null] - Retry duration in milliseconds.
       */

      /**
       * Sends a remove fragments event.
       *
       * @param {string} selector - CSS selector of fragments to remove.
       * @param {RemoveFragmentsOptions} options - Additional options for removing.
       * @throws Will throw an error if selector is missing.
       */
      RemoveFragments: function (selector, options) {
        let dataLines = [];
        if (selector) {
          dataLines.push(`selector ${selector}`);
        } else {
          throw Error("RemoveFragments missing selector.");
        }
        if (options?.settleDuration != null)
          dataLines.push(`settleDuration ${options.settleDuration}`);
        if (options?.useViewTransition != null)
          dataLines.push(`useViewTransition ${options.useViewTransition}`);
        return this._send(`datastar-remove-fragments`, dataLines, {
          eventId: options.eventId,
          retryDuration: options.retryDuration,
        });
      },
      /**
       * @typedef {Object} MergeSignalsOptions
       * @property {boolean} [onlyIfMissing] - Merge only if signals are missing.
       * @property {?string} [eventId=null] - Event ID to attach.
       * @property {?number} [retryDuration=null] - Retry duration in milliseconds.
       */

      /**
       * Sends a merge signals event.
       *
       * @param {object} signals - Signals to merge.
       * @param {MergeSignalsOptions} options - Additional options for merging.
       * @throws Will throw an error if signals are missing.
       */
      MergeSignals: function (signals, options) {
        let dataLines = [];
        if (options?.onlyIfMissing === true) {
          dataLines.push(`onlyIfMissing true`);
        }
        if (signals) {
          dataLines.push(`signals ${JSON.stringify(signals)}`);
        } else {
          throw Error("MergeSignals missing signals.");
        }
        return this._send(`datastar-merge-signals`, dataLines, {
          eventId: options.eventId,
          retryDuration: options.retryDuration,
        });
      },
      /**
       * Sends a remove signals event.
       *
       * @param {string[]} paths - Paths of signals to remove.
       * @param {SendOptions} options - Additional options for removing signals.
       * @throws Will throw an error if paths are missing.
       */
      RemoveSignals: function (paths, options) {
        let dataLines = [];
        if (paths) {
          paths
            .map((path) => {
              dataLines.push(`paths ${path}`);
            })
            .join("");
        } else {
          throw Error("RemoveSignals missing paths");
        }
        return this._send(`datastar-remove-signals`, dataLines, {
          eventId: options.eventId,
          retryDuration: options.retryDuration,
        });
      },
      /**
       * @typedef {Object} ExecuteScriptOptions
       * @property {boolean} [autoRemove] - Automatically remove the script after execution.
       * @property {?string} [eventId=null] - Event ID to attach.
       * @property {?number} [retryDuration=null] - Retry duration in milliseconds.
       */

      /**
       * Executes a script on the client-side.
       *
       * @param {string} script - Script code to execute.
       * @param {ExecuteScriptOptions} options - Additional options for execution.
       */
      ExecuteScript: function (script, options) {
        let dataLines = [];
        if (options?.autoRemove != null)
          dataLines.push(`autoRemove ${options.autoRemove}`);
        if (script) {
          dataLines.push(`script ${script}`);
        }
        return this._send(`datastar-execute-script`, dataLines, {
          eventId: options.eventId,
          retryDuration: options.retryDuration,
        });
      },
    };
  },
};
