import json
import csv
import os

def parse_name_field(name):
    """
    Parse Criterion's benchmark name format:
    e.g., 'VMemoized_v1: reachingDefinitions/Deep Loop'
    into Interpreter, Version (v1/v2), Analysis, Program
    """
    try:
        interpreter_version, rest = name.split(": ")
        if "_v1" in interpreter_version:
            version = "v1"
            interpreter = interpreter_version.replace("_v1", "").strip()
        elif "_v2" in interpreter_version:
            version = "v2"
            interpreter = interpreter_version.replace("_v2", "").strip()
        else:
            version = ""
            interpreter = interpreter_version.strip()

        analysis, program = rest.split("/")
        return interpreter.strip(), analysis.strip(), program.strip(), version
    except ValueError:
        return "", "", name, ""

# Paths
json_path = "benchmark_output/benchmark.json"
csv_path = "benchmark_output/runtime_metrics.csv"

if not os.path.exists(json_path):
    print(f"JSON file not found: {json_path}")
    exit(1)

# Read JSON data (Criterion format: [ "criterion", "version", [ benchmarkReports... ] ])
with open(json_path, "r") as f:
    data = json.load(f)

benchmark_reports = data[2] if len(data) > 2 else []

# Write CSV output
with open(csv_path, "w", newline="") as csvfile:
    fieldnames = ['Interpreter', 'Analysis', 'Program', 'Version', 'Mean_ms', 'StdDev_ms', 'OutlierVar_ms']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()

    for report in benchmark_reports:
        report_name = report.get("reportName", "")
        analysis = report.get("reportAnalysis", {})

        interpreter, analysisName, programName, version = parse_name_field(report_name)

        mean_ms = analysis.get("anMean", {}).get("estPoint", 0.0) * 1000
        stddev_ms = analysis.get("anStdDev", {}).get("estPoint", 0.0) * 1000
        outlier_var_ms = analysis.get("anOutlierVar", {}).get("ovFraction", 0.0) * 1000

        writer.writerow({
            "Interpreter": interpreter,
            "Analysis": analysisName,
            "Program": programName,
            "Version": version,
            "Mean_ms": f"{mean_ms:.3f}",
            "StdDev_ms": f"{stddev_ms:.3f}",
            "OutlierVar_ms": f"{outlier_var_ms:.3f}"
        })

print(f"CSV written to {csv_path}")
