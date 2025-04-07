# 專案成就展示區

於食藥戰情中心擔任資料分析師期間，用於專案項目數據分析之程式碼存放此處，以下進行說明：

### :file_folder: BPI
針對3種農產品類別之進口報單風險預測模型，進行模型資料調整評估，包含 **資料擷取期間** 與 **放入高風險特徵** 。
- 評估蔬菜（vegetable）、水果（fruit）及香辛料（spices）3個農產品類別。
- 高風險特徵包含高風險生產國、進口商、產品，初步以不合格率前25%作為高風險門檻。而產品包含以貨品分類號列、標準化名稱2種歸類方式。
- 依步驟區分檔案：
    1. 資料前處理：進行資料型態轉換，建立未放入與放入不同高風險特徵之資料集，存成 `.Rda` 檔。
    2. 訓練與測試集切分：切分訓練與測試集，並對訓練集進行資料不平衡處理。
    3. 資料擷取期間評估：參考農藥殘留法規變動時間點，建立不同資料擷取期間的訓練集，用以建模並找出最佳資料擷取期間。
    4. 放入高風險特徵評估：將未放入與放入不同高風險特徵之訓練集分別建模並相互比較，找出可提升模型預測效益之高風險特徵。

### :file_folder: ExpFood
