const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
    entry: ['babel-polyfill', './src/index.js'],
    output: {
        filename: '[name].js',
        path: __dirname + '/dist',
        chunkFilename: '[id].[chunkhash].js'
    },
    module: {
        rules: [
            {
                test: /\.(js|jsx)$/,
                exclude: /node_modules/,
                use: {
                    loader: "babel-loader",
                    options: {
                        presets: ['@babel/preset-env']
                    }
                }
            },
            {
                test: /\.html$/,
                use: [
                    {
                        loader: "html-loader"
                    }
                ]
            }
        ]
    },
    devtool: "inline-source-map",
    plugins: [
        new HtmlWebpackPlugin({
            template: './src/index.html',
            inject: true,
            minify: {
                removeComments: true,
                collapseWhitespace: true
            },
        })
    ]
};