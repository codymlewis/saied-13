#!/usr/bin/env Rscript

source("Report.r")

# Define the various node classes
#
# Author: Cody Lewis
# Date: 2019-05-01

# A generic node class
Node <- setRefClass(
        "Node",
        fields=list(
            id="numeric",
            service="numeric",
            capability="numeric",
            noteacc="numeric",
            QR="numeric",
            time.QR="numeric",
            reports="list",
            reputation="numeric",
            trust="numeric"
        ),
        methods=list(
            take.note = function(target.service, target.capability, proxy.service, proxy.capability, time) {
                "Take note of the Quality of the service provided by a proxy"
                note = find.note(target.service, target.capability, proxy.service, proxy.capability, time)
                if(runif(1) > noteacc) {
                    wrong_vals = setdiff(c(-1, 0, 1), note)
                    return(`if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2]))
                }
                return(note)
            },
            make.report = function(proxy, target.service, target.capability, time) {
                "Create a report on the proxy server"
                proxy$reports[length(proxy$reports) + 1] <- Report(
                    service=target.service,
                    capability=target.capability,
                    time=time,
                    note=take.note(target.service, target.capability, proxy$service, proxy$capability, time),
                    issuer=id,
                    issuer.QR=QR[[1]],
                    issuer.time.QR=time.QR[[1]]
                )
            }#,
            # flatten.reports = function(time.current) {
            #     "Remove the irrelevant reports"
            # }
        )
)

# A Bad mouthing node
Node.BadMouther <- setRefClass(
    "Node.BadMouther",
    contains="Node",
    methods=list(
        take.note = function(target.service, target.capability, proxy.service, proxy.capability, time) {
            "Take a bad mouthing note, -1"
            return(-1)
        }
    )
)

# A service setting node
Node.BadMouther.ServiceSetter <- setRefClass(
    "Node.BadMouther.ServiceSetter",
    contains="Node.BadMouther",
    methods=list(
        make.report = function(proxy, target.service, target.capability, time) {
            "Make a service setted report, service = 50"
            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=50,
                capability=target.capability,
                time=time,
                note=take.note(target.service, target.capability, proxy$service, proxy$capability, time),
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]]
            )
        }
    )
)

# A capability setting node
Node.BadMouther.CapabilitySetter <- setRefClass(
    "Node.BadMouther.CapabilitySetter",
    contains="Node.BadMouther",
    methods=list(
        make.report = function(proxy, target.service, target.capability, time) {
            "Make a capability setted report, capability = 50"
            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=target.service,
                capability=50,
                time=time,
                note=take.note(target.service, target.capability, proxy$service, proxy$capability, time),
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]]
            )
        }
    )
)

# A time decaying node
Node.BadMouther.TimeDecayer <- setRefClass(
    "Node.BadMouther.TimeDecayer",
    contains="Node.BadMouther",
    methods=list(
        make.report = function(proxy, target.service, target.capability, time) {
            "Make a time decayed report, time = time - 5"
            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=target.service,
                capability=target.capability,
                time=time - 5,
                note=take.note(target.service, target.capability, proxy$service, proxy$capability, time),
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]]
            )
        }
    )
)
# A capability setting node
Node.BadMouther.CapabilitySetter.TimeDecayer <- setRefClass(
    "Node.BadMouther.CapabilitySetter.TimeDecayer",
    contains="Node.BadMouther",
    methods=list(
        make.report = function(proxy, target.service, target.capability, time) {
            "Make a capability setted and time decayed report, capability = 50, time = time - 5"
            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=target.service,
                capability=50,
                time=time - 5,
                note=take.note(target.service, target.capability, proxy$service, proxy$capability, time),
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]]
            )
        }
    )
)


# Find the suitable note
find.note <- function(target.service, target.capability, proxy.service, proxy.capability, time) {
    if(proxy.service >= target.service && proxy.capability >= target.capability) {
        return(1)
    } else if(proxy.service >= target.service || proxy.capability >= target.capability) {
        return(0)
    } else {
        return(-1)
    }
}
