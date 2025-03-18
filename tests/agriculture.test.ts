import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const farmer = accounts.get("wallet_1")!;
const investor = accounts.get("wallet_2")!;
const reviewer = accounts.get("wallet_3")!;
const contractOwner = accounts.get("deployer")!;

describe("agriculture marketplace contract", () => {
  // Test crop registration
  it("successfully registers a crop listing", () => {
    const registerCall = simnet.callPublicFn(
      "agriculture",
      "register-crop",
      [
        Cl.stringUtf8("Corn"),
        Cl.uint(1000),
        Cl.uint(10)
      ],
      farmer
    );
    expect(registerCall.result).toBeOk(Cl.bool(true));

    const getListing = simnet.callReadOnlyFn(
        "agriculture",
        "get-farmer-listing",
        [Cl.principal(farmer)],
        farmer
    );

    const expectedResponse = Cl.tuple({
        available: Cl.bool(true),
        'crop-type': Cl.stringUtf8("Corn"),
        'price-per-unit': Cl.uint(10),
        quantity: Cl.uint(1000)
    });
  
    expect(getListing.result).toBeSome(expectedResponse);
});

  // Test crop investment
  it("successfully invests in a crop", () => {
    // First register a crop
    simnet.callPublicFn(
      "agriculture",
      "register-crop",
      [
        Cl.stringUtf8("Wheat"),
        Cl.uint(500),
        Cl.uint(5)
      ],
      farmer
    );

    const investCall = simnet.callPublicFn(
      "agriculture",
      "invest-in-crop",
      [
        Cl.principal(farmer),
        Cl.uint(100)
      ],
      investor
    );
    expect(investCall.result).toBeOk(Cl.bool(true));
  });

  // Test farmer rating
  it("successfully rates a farmer", () => {
    const ratingCall = simnet.callPublicFn(
      "agriculture",
      "rate-farmer",
      [
        Cl.principal(farmer),
        Cl.uint(5),
        Cl.stringUtf8("Excellent quality crops!")
      ],
      reviewer
    );
    expect(ratingCall.result).toBeOk(Cl.bool(true));
  });

  // Test crop season registration
  it("successfully registers crop seasons", () => {
    const seasonCall = simnet.callPublicFn(
      "agriculture",
      "add-crop-season",
      [
        Cl.stringUtf8("Rice"),
        Cl.uint(3),
        Cl.uint(9)
      ],
      contractOwner
    );
    expect(seasonCall.result).toBeOk(Cl.bool(true));
  });

  // Test bulk discount setting
  it("successfully sets bulk discount", () => {
    const discountCall = simnet.callPublicFn(
      "agriculture",
      "set-bulk-discount",
      [
        Cl.uint(1000),
        Cl.uint(10)
      ],
      farmer
    );
    expect(discountCall.result).toBeOk(Cl.bool(true));
  });

  // Test insurance purchase
  it("successfully purchases insurance", () => {
    const insuranceCall = simnet.callPublicFn(
      "agriculture",
      "purchase-insurance",
      [
        Cl.principal(farmer),
        Cl.uint(1000)
      ],
      investor
    );
    expect(insuranceCall.result).toBeOk(Cl.bool(true));
  });

  // Test organic certification
  it("successfully certifies organic status", () => {
    const certifyCall = simnet.callPublicFn(
      "agriculture",
      "certify-organic",
      [
        Cl.principal(farmer),
        Cl.uint(100000)
      ],
      contractOwner
    );
    expect(certifyCall.result).toBeOk(Cl.bool(true));
  });

  // Test escrow creation
  it("successfully creates escrow", () => {
    const escrowCall = simnet.callPublicFn(
      "agriculture",
      "create-escrow",
      [
        Cl.principal(farmer),
        Cl.uint(500)
      ],
      investor
    );
    expect(escrowCall.result).toBeOk(Cl.bool(true));
  });

  // Test error cases
  it("fails when invalid rating is provided", () => {
    const invalidRatingCall = simnet.callPublicFn(
      "agriculture",
      "rate-farmer",
      [
        Cl.principal(farmer),
        Cl.uint(6),
        Cl.stringUtf8("Invalid rating")
      ],
      reviewer
    );
    expect(invalidRatingCall.result).toBeErr(Cl.uint(105));
  });

  it("fails when unauthorized user attempts certification", () => {
    const unauthorizedCall = simnet.callPublicFn(
      "agriculture",
      "certify-organic",
      [
        Cl.principal(farmer),
        Cl.uint(100000)
      ],
      farmer // Not contract owner
    );
    expect(unauthorizedCall.result).toBeErr(Cl.uint(100));
  });
});
