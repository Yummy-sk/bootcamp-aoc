let readFile = (path) => {
  // dirname을 가져온다.  
  let dirname = %raw(`(function () { 
    const path = require('path');
    
    return path.parse(__dirname).dir;
    })()`);

  // 프로젝트 루트 경로를 가져온다.
  let parsedRootPath = dirname->Js.String2.replaceByRe(%re("/lib\/js/g"), "");
  // 받은 path에 "/"를 제거한다.
  let restOfPath = path->Js.String2.trim->Js.String2.replaceByRe(%re("/^\//"), "");
  // 전체 경로를 만든다.
  let fullPath = `${parsedRootPath}/${restOfPath}`;

  // 파일을 읽어 리턴한다.
  Node.Fs.readFileAsUtf8Sync(fullPath)
  ->Js.String2.trim;
}
