declare module "spel2scalajs" {
  export const Spel2ScalaJs: {
    evaluate: (expression: string, context: any) => any;
  };
}
