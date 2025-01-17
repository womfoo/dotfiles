package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"regexp"
)

func UpdateHash(newhash string) {
	filePath := "flake.nix"

	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}

	re := regexp.MustCompile(`(?m)^ +nixpkgs.url = "github:NixOS\/nixpkgs\/.*$`) // Replace this with your regex

	updatedContent := re.ReplaceAllString(string(content), "    nixpkgs.url = \"github:NixOS/nixpkgs/"+newhash+"\";")

	err = ioutil.WriteFile(filePath, []byte(updatedContent), 0644)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("nixpkgs hash updated to " + newhash + "successfully")
}
