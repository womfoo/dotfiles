package main

import (
	"fmt"
	"github.com/PuerkitoBio/goquery"
	"log"
	"net/http"
	"time"
)

type HydraEval struct {
	URL              string
	GitCommit        string
	SuccessfulBuilds int
	FailedBuilds     int
	Datetime         time.Time
}

var evals []HydraEval

func main() {
	url := "https://hydra.nixos.org/jobset/nixos/trunk-combined"
	// url := "http://localhost:9000/fakehydra/trunk-combined"
	resp, err := http.Get(url)
	if err != nil {
		log.Fatalf("Error fetching the page: %v\n", err)
	}
	defer resp.Body.Close()
	doc, err := goquery.NewDocumentFromReader(resp.Body)
	if err != nil {
		log.Fatalf("Error parsing the HTML: %v\n", err)
	}
	doc.Find("table.table.table-condensed.table-striped.clickable-rows").Each(func(i int, tableHtml *goquery.Selection) {
		tableHtml.Find("tr").Each(func(i int, rowHtml *goquery.Selection) {
			url, exists := rowHtml.Find("td a.row-link").Attr("href")
			if !exists {
				return
			}
			gitCommit := rowHtml.Find("td tt").Text()
			successfulBuildsText := rowHtml.Find("td span.badge-success").Text()
			var successfulBuilds int
			fmt.Sscanf(successfulBuildsText, "%d", &successfulBuilds)
			failedBuildsText := rowHtml.Find("td span.badge-danger").Text()
			var failedBuilds int
			fmt.Sscanf(failedBuildsText, "%d", &failedBuilds)
			rawdatetime, exists := rowHtml.Find("td time").Attr("datetime")
			if !exists {
				return
			}
			latestDatetime, err := time.Parse(time.RFC3339, rawdatetime)
			if err != nil {
				fmt.Println("Error parsing datetime:", err)
				return
			}

			eval := HydraEval{
				URL:              url,
				GitCommit:        gitCommit,
				SuccessfulBuilds: successfulBuilds,
				FailedBuilds:     failedBuilds,
				Datetime:         latestDatetime,
			}
			evals = append(evals, eval)
		})
	})
	for _, eval := range evals {
		if eval.SuccessfulBuilds >= 8000 {
			UpdateHash(eval.GitCommit)
			break
		}
	}
}
