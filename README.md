# 1. 主題
CAT模擬研究，複現 Some New Item Selection Criteria for Adaptive Testing , Veerkamp, W.J.J. ,1997，同時增加了新的指標，刪除了過時的指標

# 2. 文件介紹
- ## **CAT模擬研究--平行運算**：完整的模擬研究的代碼
  ### **詳細介紹**：此代碼由1+1+1*3+1，四個部分組成
  - 第一部分-生成資料與輸出：設定情境(均勻或常態/題庫200或題庫400)/設定重複次數R=?/設定施測題長，以及生成作答反應，同時將作答反應與試題參數輸出
  - 第二部分-平行運算設置代碼：分配運行要用的核心數，與進度條的設置
  - 第三部分(3個小部分)-資料生成代碼：總共有三個小部分，每一個小部分由一個準則搭配一個資料輸出代碼，具體細節與差異請結合文獻查看
  - 第四部份-圖片展示與輸出代碼：同**CAT模擬研究--畫圖**的第四部份
  ### 補充：預設為均勻分布/400題庫的情境，如果想要換別的情境，那第一部份與第四部份中(3X2個小部分)，各個輸出資料的名稱也需要更改

- ## **CAT模擬研究--畫圖**：專門用來畫圖的代碼
  ### **詳細介紹**：此代碼有四個部分
  - 第一部分-讀取資料：讀取xlsx的檔案
  - 第二部分-測試用代碼：可以無視，用於測試參數的設置
  - 第三部分-二準則代碼：可以無視，因為MLWI的資料需要跑很久，所以會用到這份代碼
  - 第四部份-全準則代碼：主要使用這一部份
  ### 補充：儘管CAT模擬研究--平行運算中已經有類似的代碼，但因為此研究需要稍長的運算時間(通常需要幾天的時間完成)，所以最好是將輸出的資料單獨提出來處理
