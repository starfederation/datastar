package datastar

import (
	"strings"

	"github.com/CAFxX/httpcompression/contrib/andybalholm/brotli"
	"github.com/CAFxX/httpcompression/contrib/compress/gzip"
	"github.com/CAFxX/httpcompression/contrib/compress/zlib"
	"github.com/CAFxX/httpcompression/contrib/klauspost/zstd"
	zstd_opts "github.com/klauspost/compress/zstd"

	"github.com/CAFxX/httpcompression"
)

// CompressionStrategy indicates the strategy for selecting the compression algorithm.
type CompressionStrategy string

const (
	// ClientPriority indicates that the client's preferred compression algorithm
	//  should be used if possible.
	ClientPriority CompressionStrategy = "client_priority"

	// ServerPriority indicates that the server's preferred compression algorithm
	//  should be used.
	ServerPriority CompressionStrategy = "server_priority"

	// Forced indicates that the first provided compression
	// algorithm must be used regardless of client or server preferences.
	Forced CompressionStrategy = "forced"
)

// Compressor pairs a [httpcompression.CompressorProvider]
// with an encoding HTTP content type.
type Compressor struct {
	Encoding   string
	Compressor httpcompression.CompressorProvider
}

// compressionOptions holds all the data for server-sent events
// message compression configuration initiated by [CompressionOption]s.
type compressionOptions struct {
	CompressionStrategy CompressionStrategy
	ClientEncodings     []string
	Compressors         []Compressor
}

// CompressionOption configures server-sent events
// message compression.
type CompressionOption func(*compressionOptions)

// GzipOption configures the Gzip compression algorithm.
type GzipOption func(*gzip.Options)

// WithGzipLevel determines the algorithm's compression level.
// Higher values result in smaller output at the cost of higher CPU usage.
//
// Choose one of the following levels:
//   - [gzip.NoCompression]
//   - [gzip.BestSpeed]
//   - [gzip.BestCompression]
//   - [gzip.DefaultCompression]
//   - [gzip.HuffmanOnly]
func WithGzipLevel(level int) GzipOption {
	return func(opts *gzip.Options) {
		opts.Level = level
	}
}

// WithGzip appends a [Gzip] compressor to the list of compressors.
//
// [Gzip]: https://en.wikipedia.org/wiki/Gzip
func WithGzip(opts ...GzipOption) CompressionOption {
	return func(cfg *compressionOptions) {
		// set default options
		options := gzip.Options{
			Level: gzip.DefaultCompression,
		}
		// Apply all provided options.
		for _, opt := range opts {
			opt(&options)
		}

		gzipCompressor, _ := gzip.New(options)

		compressor := Compressor{
			Encoding:   gzip.Encoding,
			Compressor: gzipCompressor,
		}

		cfg.Compressors = append(cfg.Compressors, compressor)
	}
}

// DeflateOption configures the Deflate compression algorithm.
type DeflateOption func(*zlib.Options)

// WithDeflateLevel determines the algorithm's compression level.
// Higher values result in smaller output at the cost of higher CPU usage.
//
// Choose one of the following levels:
//   - [zlib.NoCompression]
//   - [zlib.BestSpeed]
//   - [zlib.BestCompression]
//   - [zlib.DefaultCompression]
//   - [zlib.HuffmanOnly]
func WithDeflateLevel(level int) DeflateOption {
	return func(opts *zlib.Options) {
		opts.Level = level
	}
}

// WithDeflateDictionary sets the dictionary used by the algorithm.
// This can improve compression ratio for repeated data.
func WithDeflateDictionary(dict []byte) DeflateOption {
	return func(opts *zlib.Options) {
		opts.Dictionary = dict
	}
}

// WithDeflate appends a [Deflate] compressor to the list of compressors.
//
// [Deflate]: https://en.wikipedia.org/wiki/Deflate
func WithDeflate(opts ...DeflateOption) CompressionOption {
	return func(cfg *compressionOptions) {
		options := zlib.Options{
			Level: zlib.DefaultCompression,
		}

		for _, opt := range opts {
			opt(&options)
		}

		zlibCompressor, _ := zlib.New(options)

		compressor := Compressor{
			Encoding:   zlib.Encoding,
			Compressor: zlibCompressor,
		}

		cfg.Compressors = append(cfg.Compressors, compressor)
	}
}

// BrotliOption configures the Brotli compression algorithm.
type BrotliOption func(*brotli.Options)

// WithBrotliLevel determines the algorithm's compression level.
// Higher values result in smaller output at the cost of higher CPU usage.
// Fastest compression level is 0. Best compression level is 11.
// Defaults to 6.
func WithBrotliLevel(level int) BrotliOption {
	return func(opts *brotli.Options) {
		opts.Quality = level
	}
}

// WithBrotliLGWin the sliding window size for Brotli compression
// algorithm. Select a value between 10 and 24.
// Defaults to 0, indicating automatic window size selection based on compression quality.
func WithBrotliLGWin(lgwin int) BrotliOption {
	return func(opts *brotli.Options) {
		opts.LGWin = lgwin
	}
}

// WithBrotli appends a [Brotli] compressor to the list of compressors.
//
// [Brotli]: https://en.wikipedia.org/wiki/Brotli
func WithBrotli(opts ...BrotliOption) CompressionOption {
	return func(cfg *compressionOptions) {
		options := brotli.Options{
			Quality: brotli.DefaultCompression,
		}

		for _, opt := range opts {
			opt(&options)
		}

		brotliCompressor, _ := brotli.New(options)

		compressor := Compressor{
			Encoding:   brotli.Encoding,
			Compressor: brotliCompressor,
		}

		cfg.Compressors = append(cfg.Compressors, compressor)
	}
}

// WithZstd appends a [Zstd] compressor to the list of compressors.
//
// [Zstd]: https://en.wikipedia.org/wiki/Zstd
func WithZstd(opts ...zstd_opts.EOption) CompressionOption {
	return func(cfg *compressionOptions) {
		zstdCompressor, _ := zstd.New(opts...)

		compressor := Compressor{
			Encoding:   zstd.Encoding,
			Compressor: zstdCompressor,
		}

		cfg.Compressors = append(cfg.Compressors, compressor)
	}
}

// WithClientPriority sets the compression strategy to [ClientPriority].
// The compression algorithm will be selected based on the
// client's preference from the list of included compressors.
func WithClientPriority() CompressionOption {
	return func(cfg *compressionOptions) {
		cfg.CompressionStrategy = ClientPriority
	}
}

// WithServerPriority sets the compression strategy to [ServerPriority].
// The compression algorithm will be selected based on the
// server's preference from the list of included compressors.
func WithServerPriority() CompressionOption {
	return func(cfg *compressionOptions) {
		cfg.CompressionStrategy = ServerPriority
	}
}

// WithForced sets the compression strategy to [Forced].
// The first compression algorithm will be selected
// from the list of included compressors.
func WithForced() CompressionOption {
	return func(cfg *compressionOptions) {
		cfg.CompressionStrategy = Forced
	}
}

// WithCompression adds compression to server-sent event stream.
func WithCompression(opts ...CompressionOption) SSEOption {
	return func(sse *ServerSentEventGenerator) {
		cfg := &compressionOptions{
			CompressionStrategy: ClientPriority,
			ClientEncodings:     parseEncodings(sse.acceptEncoding),
		}

		// apply options
		for _, opt := range opts {
			opt(cfg)
		}

		// set defaults
		if len(cfg.Compressors) == 0 {
			WithBrotli()(cfg)
			WithZstd()(cfg)
			WithGzip()(cfg)
			WithDeflate()(cfg)
		}

		switch cfg.CompressionStrategy {
		case ClientPriority:
			for _, clientEnc := range cfg.ClientEncodings {
				for _, comp := range cfg.Compressors {
					if comp.Encoding == clientEnc {
						sse.w = comp.Compressor.Get(sse.w)
						sse.encoding = comp.Encoding
						return
					}
				}
			}
		case ServerPriority:
			for _, comp := range cfg.Compressors {
				for _, clientEnc := range cfg.ClientEncodings {
					if comp.Encoding == clientEnc {
						sse.w = comp.Compressor.Get(sse.w)
						sse.encoding = comp.Encoding
						return
					}
				}
			}
		case Forced:
			if len(cfg.Compressors) > 0 {
				sse.w = cfg.Compressors[0].Compressor.Get(sse.w)
				sse.encoding = cfg.Compressors[0].Encoding
			}
		}
	}
}

func parseEncodings(header string) []string {
	parts := strings.Split(header, ",")
	var tokens []string
	for _, part := range parts {
		token := strings.SplitN(strings.TrimSpace(part), ";", 2)[0]
		if token != "" {
			tokens = append(tokens, token)
		}
	}
	return tokens
}
