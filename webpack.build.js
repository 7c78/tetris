let path = require("path")
let HtmlWebpackPlugin = require("html-webpack-plugin")
let CopyPlugin = require("copy-webpack-plugin")

module.exports = {
    mode: "production",
    entry: "./src/App.fsproj",
    output: {
        path: path.join(__dirname, "./build"),
        filename: "app.js"
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: "fable-loader"
            }
        ]
    },
    plugins: [
        new CopyPlugin({
            patterns: [
                { from: "src/app.css", to: "app.css" }
            ]
        }),

        new HtmlWebpackPlugin({
            filename: "./index.html",
            template: "./src/index.html"
        })
    ]
}
