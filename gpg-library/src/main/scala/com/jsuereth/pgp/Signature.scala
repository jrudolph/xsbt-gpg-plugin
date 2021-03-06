package com.jsuereth.pgp

import org.bouncycastle.bcpg._
import org.bouncycastle.openpgp._

/** Wrapper around a PGP signature for convenience. */
class Signature(val nested: PGPSignature) {
  /** Returns the name-value string pairs in the notation data occurrences of a signature. */
  // TODO - return a map
  // TODO - Ensure string->string is ok for all returned values...
  object notations extends Traversable[(String,String)] {
    override def foreach[U](f: ((String,String)) => U): Unit = 
      for {
        data <- nested.getHashedSubPackets.getNotationDataOccurences
      } f(data.getNotationName() -> data.getNotationValue())
  }
  
  def keyID = nested.getKeyID
  def issuerKeyID = nested.getHashedSubPackets.getIssuerKeyID
  def keyExpirationTime = nested.getHashedSubPackets.getKeyExpirationTime
  def signerUserID = nested.getHashedSubPackets.getSignerUserID
  
  override def toString = 
    "Signature(key=%x,user=%s,notations=%s)" format (
        keyID, 
        signerUserID, 
        notations map { case (k,v) => k + " -> " + v } mkString ",")
}

object Signature {
  def apply(sig: PGPSignature): Signature = new Signature(sig)
  implicit def unwrap(sig: Signature): PGPSignature = sig.nested
}