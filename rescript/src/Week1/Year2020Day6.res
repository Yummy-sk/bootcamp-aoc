/**
 * parseInput
 * @description 입력받은 입력 값에 대해 파싱을 수행합니다.
 *
 * [input]          [output]
 *   abc
 *
 *    a  ->  [['abc'], ['a', 'b', 'c']]
 *    b
 *    c
 */
type parseInputType = () => array<array<string>>
let parseInput: parseInputType = () => {
  Node.Fs.readFileAsUtf8Sync("../../../../input/Week1/Year2020Day6.sample.txt") 
    ->Js.String2.trim
    ->Js.String2.split("\n\n")
    ->Js.Array2.map((str) => str->Js.String2.split("\n"))
}


/**
 * splitQuestions
 * @description 입력받은 입력 값에 대해 질문을 분리합니다.
 *
 *  [input]
 *  [['abc'], ['a', 'b', 'c']]
 *
 *  [output]
 *  [[['a'], ['b'], ['c']], [['a'], ['b'], ['c']]]
 */
type splitQuestionsType = array<array<string>> => array<array<array<string>>>
let splitQuestions: splitQuestionsType = (questions) => 
  questions
    ->Belt.Array.map(
      (question) =>
        question
          ->Belt.Array.map((str) => str->Js.String2.split(""))
    )


/**
 * collectQuestion
 * @description 여러 질문을 하나로 concat 합니다.
 *
 * [input]
 * [[['a'], ['b'], ['c']], [['a'], ['b'], ['c']]]
 *
 * [output]
 * [['a', 'b', 'c'], ['a', 'b', 'c']]
 */
type collectQuestionType = array<array<string>> => array<string>
let collectQuestion: collectQuestionType = (question) => question->Belt.Array.concatMany


type collectQuestionsType = array<array<array<string>>> => array<array<string>>
let collectQuestions: collectQuestionsType = (questions) => {
    questions
      ->Belt.Array.map(collectQuestion)
}


/**
 * getUniqueQuestion
 * @description 배열의 값 중 고유의 값을 만듭니다.
 *
 * [input]
 * ['a', 'b', 'c', 'a']
 *
 * [output]
 * ['a', 'b', 'c']
 */
type getUniqueQuestionType = array<string> => array<string>
let getUniqueQuestion: getUniqueQuestionType = (question) =>       
  question
    ->Belt.Set.String.fromArray
    ->Belt.Set.String.toArray


type getUniqueQuestionsType = array<array<string>> => array<array<string>>
let getUniqueQuestions: getUniqueQuestionsType = (questions) => 
  questions
    ->Belt.Array.map(getUniqueQuestion)


/**
 * getCountOfQuestions
 * @description 배열의 값 중 고유의 값을 만들고, 각 고유의 값의 개수를 세어 객체로 반환합니다.
 *
 * [input]
 * [['a', 'b', 'c', 'a'], ['a', 'b', 'a']]
 *
 * [output]
 * [4, 3]
 */
type getCountOfQuestionsType = array<array<string>> => array<int>
let getCountOfQuestions: getCountOfQuestionsType = (questions) => 
  questions
    ->Belt.Array.map((question) => question->Belt.Array.length)


/**
 * getInfoOfGroup
 * @description 각 그룹의 정보를 반환합니다. 정보에는 사람 수, 그룹의 질문을 포함합니다.
 *
 * [input]
 * [[['a'], ['a', 'b']], [['a', 'b', 'c']]]
 *
 * [output]
 * [{num: 2, ques: ['a', 'a', 'b']}, {num: 1, ques: ['a', 'b', 'c']}]
 */
type info = {
    num: int,
    ques: array<string>,
}
type getInfoOfGroupType = array<array<array<string>>> => array<info>
let getInfoOfGroup: getInfoOfGroupType = (questions) => {
  questions
    ->Belt.Array.map((question) => {
        let numberOfPeople = question->Belt.Array.length
        let collectedQuestion = question->collectQuestion

        ({
            num: numberOfPeople,
            ques: collectedQuestion,
        })
    })
}


/**
 * checkQuestionsAreAllAnswered
 * @description 각 값에 대해 모두 답이 되었는지의 여부를 반환합니다.
 *
 * [input]
 * [{num: 2, ques: ['a', 'a', 'b']}, {num: 1, ques: ['a', 'b', 'c']}]
 *
 * [output]
 * [[true, false], [true, true, true]]
 */
type checkQuestionsAreAllAnsweredType = array<info> => array<array<bool>>
let checkQuestionsAreAllAnswered: checkQuestionsAreAllAnsweredType = (infos) => {
  infos
    ->Belt.Array.map((info) => {
        let {num, ques} = info
        let uniqueQuestion = ques->getUniqueQuestion

        uniqueQuestion
          ->Js.Array2.map((question) => {
            let count = ques->Js.Array2.filter((q) => q == question)->Belt.Array.length

            count == num
          })          
    })
}


/**
 * countAnsweredQuestions
 * @description 각 그룹의 모두 답변된 문항의 갯수를 반환합니다.
 *
 * [input]
 * [[true, false], [true, true, true]]
 *
 * [output]
 * [1, 3]
 */
type countAnsweredQuestionsType = array<array<bool>> => array<int>
let countAnsweredQuestions: countAnsweredQuestionsType = (questions) => {
  questions
    ->Belt.Array.map((question) => {
        question
          ->Js.Array2.filter((q) => q)
          ->Belt.Array.length
    })
}


type getSumOfCountsType = array<int> => int
let getSumOfCounts: getSumOfCountsType = (counts) => 
  counts
    ->Belt.Array.reduce(0, (acc, count) => acc + count)


let solutionPart1 = () => 
  parseInput()
    ->splitQuestions
    ->collectQuestions
    ->getUniqueQuestions
    ->getCountOfQuestions
    ->getSumOfCounts
    ->Js.log
  

let solutionPart2 = () => 
  parseInput()
    ->splitQuestions
    ->getInfoOfGroup
    ->checkQuestionsAreAllAnswered
    ->countAnsweredQuestions
    ->getSumOfCounts
    ->Js.log


solutionPart1()
solutionPart2()
