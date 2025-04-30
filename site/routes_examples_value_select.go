package site

import (
	"net/http"

	"github.com/delaneyj/toolbelt"
	"github.com/go-chi/chi/v5"
	"github.com/samber/lo"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesValueSelect(examplesRouter chi.Router) error {
	cars := []*ValueSelectMake{
		{
			ID:    toolbelt.NextEncodedID(),
			Label: "Audi",
			Models: []*ValueSelectModel{
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "A1",
				},
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "A3",
				},
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "A6",
				},
			},
		},
		{
			ID:    toolbelt.NextEncodedID(),
			Label: "Toyota",
			Models: []*ValueSelectModel{
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "Land Cruiser",
				},
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "Corolla",
				},
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "Camry",
				},
			},
		},
		{
			ID:    toolbelt.NextEncodedID(),
			Label: "Ford",
			Models: []*ValueSelectModel{
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "F-150",
				},
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "Mustang",
				},
				{
					ID:    toolbelt.NextEncodedID(),
					Label: "Focus",
				},
			},
		},
	}

	signalsValidation := func(signals *ValueSelectSignals) (make *ValueSelectMake, model *ValueSelectModel, isValid bool) {
		if signals.Make != "" {
			for _, possibleMake := range cars {
				if possibleMake.ID == signals.Make {
					make = possibleMake

					if signals.Model != "" {
						for _, possibleModel := range make.Models {
							if possibleModel.ID == signals.Model {
								model = possibleModel
								isValid = true
							}
						}
					}
					break
				}
			}
		}

		return make, model, isValid
	}

	examplesRouter.Route("/value_select/data", func(dataRouter chi.Router) {

		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &ValueSelectSignals{}
			if err := datastar.ReadSignals(r, signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			make, model, isValid := signalsValidation(signals)

			c := valueSelectView(cars, signals, make, model, isValid)
			datastar.NewSSE(w, r).MergeFragmentTempl(c)
		})

		dataRouter.Post("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &ValueSelectSignals{}
			if err := datastar.ReadSignals(r, signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			_, _, isValid := signalsValidation(signals)

			if !isValid {
				http.Error(w, "invalid input", http.StatusBadRequest)
				return
			}

			sse := datastar.NewSSE(w, r)

			make, ok := lo.Find(cars, func(item *ValueSelectMake) bool {
				return item.ID == signals.Make
			})
			if !ok {
				http.Error(w, "invalid input", http.StatusBadRequest)
				return
			}

			model, ok := lo.Find(make.Models, func(item *ValueSelectModel) bool {
				return item.ID == signals.Model
			})

			if !ok {
				http.Error(w, "invalid input", http.StatusBadRequest)
				return
			}

			sse.MergeFragmentTempl(valueSelectResults(make, model))
		})
	})

	return nil
}
