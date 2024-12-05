import Foundation

func getLines(_ fileName: String) -> [String] {
    do {
        let url = URL(fileURLWithPath: fileName)
        let text = try String(contentsOf: url, encoding: .utf8)
        return text.split(separator: "\n", omittingEmptySubsequences: false).map(String.init)
    } catch {
        return []
    }
}

// https://stackoverflow.com/a/29835826/2990344

extension StringProtocol {
    var asciiValues: [UInt8] { compactMap(\.asciiValue) }
}

//https://docs.swift.org/swift-book/documentation/the-swift-programming-language/stringsandcharacters/#String-Indices
// "Swift strings can’t be indexed by integer values."
