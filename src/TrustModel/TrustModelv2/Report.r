#!/usr/bin/env Rscript

# A reference class for a report
#
# Author: Cody Lewis
# Date: 2019-05-01

Report <- setRefClass(
    "Report",
    fields=list(
        service="numeric",
        capability="numeric",
        time="numeric",
        note="numeric",
        issuer="numeric",
        issuer.QR="numeric",
        issuer.time.QR="numeric"
    )
)

find.diff <- function(target, current) {
    return(abs(target - current))
}

report.distance <- function(report, target.service, target.capability, service.max, capability.max, eta) {
    service.max = service.max + 1
    capability.max = capability.max + 1
    dS.max.squared = find.diff(target.service, service.max)**2
    dC.max.squared = find.diff(target.capability, capability.max)**2
    term.shared = sqrt(
        (dS.max.squared + dC.max.squared) *
            ((find.diff(target.service, report$service)**2 / dS.max.squared) +
             (find.diff(target.capability, report$capability)**2 / dC.max.squared))
    )
    if(report$note >= 0) {
        term.unique = sqrt(
            (dS.max.squared + dC.max.squared) *
                (((service.max - report$service) / (service.max - (target.service - eta)))**2 +
                 (report$capability / (target.capability + eta))**2)
        )
    } else {
        term.unique = sqrt(
            (dS.max.squared + dC.max.squared) *
                (((capability.max - report$capability) / (capability.max - (target.capability - eta)))**2 +
                 (report$service / (target.service + eta))**2)
        )
    }
    return(min(term.shared, term.shared))
}

report.weigh <- function(report, dist, lambda, theta, time.current) {
    s = (1 / 2) * (report$note**2 - report$note)
    return(lambda**dist * theta**((s + 1) * (time.current - report$time)))
}
