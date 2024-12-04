import Foundation

func getLines(_ fileName: String) -> [String] {
    do {
        let url = URL(fileURLWithPath: fileName)
        let text = try String(contentsOf: url, encoding: .utf8)
        return text.split(separator: "\n").map(String.init)
    } catch {
        return []
    }
}
