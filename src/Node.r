# Define the various node classes
#
# Author: Cody Lewis
# Date: 2019-05-01

source("Report.r")

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
                 # TODO: Add note checking mechanic
        take.note = function(target.service, target.capability, proxy.service, proxy.capability, time) {
            "Take note of the Quality of the service provided by a proxy"
            note = find.note(target.service, target.capability, proxy.service, proxy.capability, time)
            if(runif(1) > noteacc) {
                wrong_vals = setdiff(c(-1, 0, 1), note)
                return(`if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2]))
            }
            return(note)
        },
        take.service = function(target.service) {
            return(target.service)
        },
        take.capability = function(proxy) {
            return(proxy$capability)
        },
        take.time = function(time) {
            return(time)
        },
        make.report = function(proxy, target.service, target.capability, time) {
            "Create a report on the proxy server"
            note = take.note(target.service, target.capability, proxy$service, proxy$capability, time)
            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=take.service(target.service),
                capability=take.capability(proxy),
                time=take.time(time),
                note=note,
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]]
            )
        },
        write.data = function() {
            params = data.frame(id=id, service=service, capability=capability, noteacc=noteacc, reputation=reputation)
            write.csv(params, "data/nodes.csv", append=TRUE)
            trustvals = data.frame(QR=QR, time.QR=time.QR, trust=trust)
            write.csv(trustvals, sprintf("data/node-%d-trust.csv", id))
            for(report in reports) {
                report$write.data(sprintf("data/node-%d-repost", id))
            }
        }
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
        take.service = function(target.service) {
            return(50)
        }
    )
)

# A capability setting node
Node.BadMouther.CapabilitySetter <- setRefClass(
    "Node.BadMouther.CapabilitySetter",
    contains="Node.BadMouther",
    methods=list(
        take.capability = function(proxy) {
            return(50)
        }
    )
)

# A time decaying node
Node.BadMouther.TimeDecayer <- setRefClass(
    "Node.BadMouther.TimeDecayer",
    contains="Node.BadMouther",
    methods=list(
        take.time = function(time) {
            return(time - 5)
        }
    )
)
# A capability setting node
Node.BadMouther.CapabilitySetter.TimeDecayer <- setRefClass(
    "Node.BadMouther.CapabilitySetter.TimeDecayer",
    contains="Node.BadMouther.CapabilitySetter",
    methods=list(
        take.time = function(time) {
            return(time - 5)
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
