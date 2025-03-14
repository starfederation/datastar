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

type CompressionStrategy string

const (
	ClientPriority = "client_priority"
	ServerPriority = "server_priority"
	Forced         = "forced"
)

type Compressor struct {
	Encoding   string
	Compressor httpcompression.CompressorProvider
}

type CompressionConfig struct {
	CompressionStrategy CompressionStrategy
	ClientEncodings     []string
	Compressors         []Compressor
}

type CompressionOption func(*CompressionConfig)

type GzipOption func(*gzip.Options)

func WithGzipLevel(level int) GzipOption {
	return func(opts *gzip.Options) {
		opts.Level = level
	}
}

func WithGzip(opts ...GzipOption) CompressionOption {
	return func(cfg *CompressionConfig) {
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

type DeflateOption func(*zlib.Options)

func WithDeflateLevel(level int) DeflateOption {
	return func(opts *zlib.Options) {
		opts.Level = level
	}
}

func WithDeflateDictionary(dict []byte) DeflateOption {
	return func(opts *zlib.Options) {
		opts.Dictionary = dict
	}
}

func WithDeflate(opts ...DeflateOption) CompressionOption {
	return func(cfg *CompressionConfig) {
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

type brotliOption func(*brotli.Options)

func WithBrotliLevel(level int) brotliOption {
	return func(opts *brotli.Options) {
		opts.Quality = level
	}
}

func WithBrotliLGWin(lgwin int) brotliOption {
	return func(opts *brotli.Options) {
		opts.LGWin = lgwin
	}
}

func WithBrotli(opts ...brotliOption) CompressionOption {
	return func(cfg *CompressionConfig) {
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

func WithZstd(opts ...zstd_opts.EOption) CompressionOption {
	return func(cfg *CompressionConfig) {

		zstdCompressor, _ := zstd.New(opts...)

		compressor := Compressor{
			Encoding:   zstd.Encoding,
			Compressor: zstdCompressor,
		}

		cfg.Compressors = append(cfg.Compressors, compressor)
	}
}

func WithClientPriority() CompressionOption {
	return func(cfg *CompressionConfig) {
		cfg.CompressionStrategy = ClientPriority
	}
}

func WithServerPriority() CompressionOption {
	return func(cfg *CompressionConfig) {
		cfg.CompressionStrategy = ServerPriority
	}
}

func WithForced() CompressionOption {
	return func(cfg *CompressionConfig) {
		cfg.CompressionStrategy = Forced
	}
}

func WithCompression(opts ...CompressionOption) SSEOption {

	return func(sse *ServerSentEventGenerator) {
		cfg := &CompressionConfig{
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
