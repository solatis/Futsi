namespace Futsi

module Types = 

    // A hostname is represented by a string
    type 'a HostName    = HostName of string

    // A Port is represented by a 32 bit integer
    type 'a Port        = Port     of int32

    // Different socket types we support
    type SocketType = 
        | VirtualStream
        | DatagramRepliable
        | DatagramAnonymous

    // A Destination is represented by a Base64 encoding
    type 'a Destination = Destination of string

    // Supported signature types by I2P, as defined at
    // <https://geti2p.net/en/docs/spec/common-structures#type_Signature I2P Common Structure Documentation>
    type SignatureType =
        | DsaSha1
        | EcdsaSha256P256
        | EcdsaSha384P384
        | EcdsaSha512P521
        | RsaSha2562048
        | RsaSha3843072
        | RsaSha5124096
        | EdDsaSha512Ed25519    