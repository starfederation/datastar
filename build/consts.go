package build

import (
	"time"

	"github.com/delaneyj/toolbelt"
)

type EnumValueDefinition struct {
	Name        toolbelt.CasedString
	Description string
	Value       string
}

type EnumDefinition struct {
	Name         toolbelt.CasedString
	Description  string
	Values       []*EnumValueDefinition
	DefaultIndex int
	Default      *EnumValueDefinition
}

type DefaultDuration struct {
	Name        toolbelt.CasedString
	Description string
	Duration    time.Duration
}

type DefaultBool struct {
	Name        toolbelt.CasedString
	Description string
	Value       bool
}

type DefaultString struct {
	Name        toolbelt.CasedString
	Description string
	Value       string
}

type Language struct {
	FileExtension string
	Name          string
	Icon          string
	SdkUrl        string
}

type ConstTemplateData struct {
	DoNotEdit                 string
	SDKLanguages              []Language
	Version                   string
	VersionClientByteSize     int
	VersionClientByteSizeGzip int
	DatastarKey               string
	DatalineLiterals          []toolbelt.CasedString
	DefaultBools              []*DefaultBool
	DefaultDurations          []*DefaultDuration
	DefaultStrings            []*DefaultString
	Enums                     []*EnumDefinition
}

var Consts = &ConstTemplateData{
	DoNotEdit: "This is auto-generated by Datastar. DO NOT EDIT.",
	SDKLanguages: []Language{
		{
			FileExtension: "go",
			Name:          "Go",
			Icon:          "vscode-icons:file-type-go-gopher",
			SdkUrl:        "https://github.com/starfederation/datastar/tree/main/sdk/go",
		},
		{
			FileExtension: "php",
			Name:          "PHP",
			Icon:          "vscode-icons:file-type-php2",
			SdkUrl:        "https://github.com/starfederation/datastar/tree/main/sdk/php",
		},
		{
			FileExtension: "python",
			Name:          "Python",
			Icon:          "vscode-icons:file-type-python",
			SdkUrl:        "https://github.com/starfederation/datastar/tree/main/sdk/python",
		},
		{
			FileExtension: "typescript",
			Name:          "TypeScript",
			Icon:          "vscode-icons:file-type-typescript-official",
			SdkUrl:        "https://github.com/starfederation/datastar/tree/main/sdk/typescript",
		},
		{
			FileExtension: "fs",
			Name:          "Dotnet",
			Icon:          "vscode-icons:file-type-fsharp2",
			SdkUrl:        "https://github.com/starfederation/datastar/tree/main/sdk/dotnet",
		},
		{
			FileExtension: "java",
			Name:          "Java",
			Icon:          "vscode-icons:file-type-java",
			SdkUrl:        "https://github.com/starfederation/datastar/tree/main/sdk/java",
		},
	},
	DatastarKey: "datastar",
	DefaultBools: []*DefaultBool{
		{
			Name:        toolbelt.ToCasedString("fragmentsUseViewTransitions"),
			Description: "Should fragments be merged using the ViewTransition API?",
			Value:       false,
		},
		{
			Name:        toolbelt.ToCasedString("mergeSignalsOnlyIfMissing"),
			Description: "Should a given set of signals merge if they are missing?",
			Value:       false,
		},
		{
			Name:        toolbelt.ToCasedString("executeScriptAutoRemove"),
			Description: "Should script element remove itself after execution?",
			Value:       true,
		},
	},
	DefaultDurations: []*DefaultDuration{
		{
			Name:        toolbelt.ToCasedString("fragmentsSettleDuration"),
			Description: "The default duration for settling during fragment merges. Allows for CSS transitions to complete.",
			Duration:    300 * time.Millisecond,
		},
		{
			Name:        toolbelt.ToCasedString("sseRetryDuration"),
			Description: "The default duration for retrying SSE on connection reset. This is part of the underlying retry mechanism of SSE.",
			Duration:    1 * time.Second,
		},
	},
	DefaultStrings: []*DefaultString{
		{
			Name:        toolbelt.ToCasedString("executeScriptAttributes"),
			Description: `The default attributes for <script/> element use when executing scripts. It is a set of key-value pairs delimited by a newline \\n character.`,
			Value:       "type module",
		},
	},
	DatalineLiterals: []toolbelt.CasedString{
		// Shared
		toolbelt.ToCasedString("selector"),

		// MergeFragments
		toolbelt.ToCasedString("mergeMode"),
		toolbelt.ToCasedString("settleDuration"),
		toolbelt.ToCasedString("fragments"),
		toolbelt.ToCasedString("useViewTransition"),

		// MergeSignals
		toolbelt.ToCasedString("signals"),
		toolbelt.ToCasedString("onlyIfMissing"),

		// RemoveSignals
		toolbelt.ToCasedString("paths"),

		// ExecuteScript
		toolbelt.ToCasedString("script"),
		toolbelt.ToCasedString("attributes"),
		toolbelt.ToCasedString("autoRemove"),
	},
	Enums: []*EnumDefinition{
		{
			Name:         toolbelt.ToCasedString("FragmentMergeMode"),
			Description:  "The mode in which a fragment is merged into the DOM.",
			DefaultIndex: 0,
			Values: []*EnumValueDefinition{
				{
					Value:       "morph",
					Description: "Morphs the fragment into the existing element using idiomorph.",
				},
				{
					Value:       "inner",
					Description: "Replaces the inner HTML of the existing element.",
				},
				{
					Value:       "outer",
					Description: "Replaces the outer HTML of the existing element.",
				},
				{
					Value:       "prepend",
					Description: "Prepends the fragment to the existing element.",
				},
				{
					Value:       "append",
					Description: "Appends the fragment to the existing element.",
				},
				{
					Value:       "before",
					Description: "Inserts the fragment before the existing element.",
				},
				{
					Value:       "after",
					Description: "Inserts the fragment after the existing element.",
				},
				{
					Value:       "upsertAttributes",
					Description: "Upserts the attributes of the existing element.",
				},
			},
		},

		{
			Name:         toolbelt.ToCasedString("EventType"),
			Description:  "The type protocol on top of SSE which allows for core pushed based communication between the server and the client.",
			DefaultIndex: -1,
			Values: []*EnumValueDefinition{
				{
					Name:        toolbelt.ToCasedString("MergeFragments"),
					Description: "An event for merging HTML fragments into the DOM.",
					Value:       "datastar-merge-fragments",
				},
				{
					Name:        toolbelt.ToCasedString("MergeSignals"),
					Description: "An event for merging signals.",
					Value:       "datastar-merge-signals",
				},
				{
					Name:        toolbelt.ToCasedString("RemoveFragments"),
					Description: "An event for removing HTML fragments from the DOM.",
					Value:       "datastar-remove-fragments",
				},
				{
					Name:        toolbelt.ToCasedString("RemoveSignals"),
					Description: "An event for removing signals.",
					Value:       "datastar-remove-signals",
				},
				{
					Name:        toolbelt.ToCasedString("ExecuteScript"),
					Description: "An event for executing <script/> elements in the browser.",
					Value:       "datastar-execute-script",
				},
			},
		},
	},
}
