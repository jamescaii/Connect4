let getInputJSLine : unit => string = {
    [%bs.raw{|
      function() {
        const readlineSync = require('readline-sync');
        const rl = readlineSync;
        var ans = rl.question('Test readline-sync by typing something and hitting enter ');
        return ans;
    }|}]
  }

let test =() : unit => {
    let input : string = getInputJSLine();
    print_endline(input);
}

test();



