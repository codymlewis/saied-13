# Define the various node classes
#
# Author: Cody Lewis
# Date: 2019-05-01

source("Report.r")

ALT1 <- 1
ALT2 <- 2

# A generic node class
Node <- setRefClass(
    "Node",
    fields=list(
        id="numeric",
        service="numeric",
        capability="numeric",
        noteacc="numeric",
        QR="numeric",
        malicious="logical",
        time.QR="numeric",
        reports="list",
        reputation="numeric",
        trust="numeric",
        type.calc="numeric",
        time.possible.attack="numeric",
        time.disregard="numeric"
    ),
    methods=list(
        initialize = function(id, service, capability, noteacc, QR, malicious, number.nodes, type.calc=0, time.disregard=1) {
            id <<- id
            service <<- service
            capability <<- capability
            noteacc <<- noteacc
            QR <<- QR
            malicious <<- malicious
            time.QR <<- 0
            type.calc <<- type.calc
            time.disregard <<- time.disregard
            if(type.calc == ALT1) {
                time.possible.attack <<- rep(NA, number.nodes)
            }
        },
        take.note = function(target.service, target.capability, proxy, time) {
            "Take note of the Quality of the service provided by a proxy"
            note = find.note(target.service, target.capability, proxy, time)
            if(runif(1) > noteacc) {
                wrong_vals = setdiff(c(-1, 0, 1), note)
                return(`if`(runif(1) < 0.5, wrong_vals[1], wrong_vals[2]))
            }
            return(note)
        },
        take.service = function(target.service) {
            "Get the service value for a report"
            return(target.service)
        },
        take.capability = function(proxy) {
            "Get the capability value for a report"
            return(proxy$capability)
        },
        take.time = function(time) {
            "Get the time value for a report"
            return(time)
        },
        make.report = function(proxy, target.service, target.capability, time) {
            "Create a report on the proxy server"
            note = take.note(target.service, target.capability, proxy, time)
            proxy$reports[length(proxy$reports) + 1] <- Report(
                service=take.service(target.service),
                capability=take.capability(proxy),
                time=take.time(time),
                note=note,
                issuer=id,
                issuer.QR=QR[[1]],
                issuer.time.QR=time.QR[[1]],
                disregard=(type.calc == ALT1 &&
                           !is.na(time.possible.attack[[id]]) &&
                           time.possible.attack[[id]] >= time - time.disregard)
            )
            if(type.calc >= ALT1 && note == -1) {
                time.possible.attack[[id]] <<- time
            }
            # search backwards through reports until time - t.d, if same context then set time.possible.attack to time
            if(type.calc >= ALT2) {
                for(i in length(proxy$reports) - 1:1) {
                    if(proxy$reports[i]$time < time - time.disregard) {
                        break
                    }
                    if(proxy$reports[i]$capability == capability || proxy$reports[i]$service == service) {
                        time.possible.attack[[id]] <<- time
                    }
                }
            }
        },
        write.data = function() {
            "Write this nodes data to a csv"
            params = data.frame(id=id, service=service, capability=capability, noteacc=noteacc, reputation=reputation)
            write.csv(params, "data/nodes.csv", append=TRUE)
            trustvals = data.frame(QR=QR, time.QR=time.QR, trust=trust)
            write.csv(trustvals, sprintf("data/node-%d-trust.csv", id))
            for(report in reports) {
                report$write.data(sprintf("data/node-%d-report", id))
            }
        }
    )
)

# A Bad mouthing node
Node.BadMouther <- setRefClass(
    "Node.BadMouther",
    contains="Node",
    methods=list(
        take.note = function(target.service, target.capability, proxy, time) {
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
            "Give a service setted service value"
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
            "Give a capability setted capability value"
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
            "Give a time decayed time value"
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
            "Give a time decayed time value"
            return(time - 5)
        }
    )
)


# Find the suitable note
find.note <- function(target.service, target.capability, proxy, time) {
    if(proxy$malicious) {
        return(-1)
    } else if(proxy$service >= target.service && proxy$capability >= target.capability) {
        return(1)
    } else {
        return(0)
    }
}
