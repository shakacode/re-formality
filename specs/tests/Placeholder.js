describe("Placeholder", () => {
  it("shows an error", () => {
    cy.visit("#Placeholder");
    cy.get("#button--submit").click();

    cy.get("#field--name--error").should("contain", "Name is required");
  });
});
