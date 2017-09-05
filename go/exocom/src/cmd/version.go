package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
)

var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Displays the version",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("0.26.1")
	},
}

func init() {
	rootCmd.AddCommand(versionCmd)
}
