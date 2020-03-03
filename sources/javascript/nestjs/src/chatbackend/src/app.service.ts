import { Injectable } from '@nestjs/common';


@Injectable()
export class AppService2 {
  getHello(name: string): string {
    return `Hello World ${name}!`;
  }
}

export abstract class IAppService
{
  abstract getHello(name: string): string;
}

@Injectable()
export class AppService  {
  constructor(
    private svc: AppService2
  ) {}
  getHello(name: string): string {
    console.log("AppService.getHello", name)
    return `Hello World ${name}!`;
  }
}

