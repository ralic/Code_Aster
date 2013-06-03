subroutine te0000(numc, opt, te)
! aslint: disable=W1501
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/te0001.h'
    include 'asterfort/te0002.h'
    include 'asterfort/te0003.h'
    include 'asterfort/te0004.h'
    include 'asterfort/te0005.h'
    include 'asterfort/te0006.h'
    include 'asterfort/te0007.h'
    include 'asterfort/te0008.h'
    include 'asterfort/te0009.h'
    include 'asterfort/te0010.h'
    include 'asterfort/te0011.h'
    include 'asterfort/te0012.h'
    include 'asterfort/te0013.h'
    include 'asterfort/te0014.h'
    include 'asterfort/te0015.h'
    include 'asterfort/te0016.h'
    include 'asterfort/te0017.h'
    include 'asterfort/te0018.h'
    include 'asterfort/te0019.h'
    include 'asterfort/te0020.h'
    include 'asterfort/te0021.h'
    include 'asterfort/te0022.h'
    include 'asterfort/te0023.h'
    include 'asterfort/te0024.h'
    include 'asterfort/te0025.h'
    include 'asterfort/te0026.h'
    include 'asterfort/te0027.h'
    include 'asterfort/te0028.h'
    include 'asterfort/te0029.h'
    include 'asterfort/te0030.h'
    include 'asterfort/te0031.h'
    include 'asterfort/te0032.h'
    include 'asterfort/te0033.h'
    include 'asterfort/te0034.h'
    include 'asterfort/te0035.h'
    include 'asterfort/te0036.h'
    include 'asterfort/te0037.h'
    include 'asterfort/te0038.h'
    include 'asterfort/te0039.h'
    include 'asterfort/te0040.h'
    include 'asterfort/te0041.h'
    include 'asterfort/te0042.h'
    include 'asterfort/te0043.h'
    include 'asterfort/te0044.h'
    include 'asterfort/te0045.h'
    include 'asterfort/te0046.h'
    include 'asterfort/te0047.h'
    include 'asterfort/te0048.h'
    include 'asterfort/te0049.h'
    include 'asterfort/te0050.h'
    include 'asterfort/te0051.h'
    include 'asterfort/te0052.h'
    include 'asterfort/te0053.h'
    include 'asterfort/te0054.h'
    include 'asterfort/te0055.h'
    include 'asterfort/te0056.h'
    include 'asterfort/te0057.h'
    include 'asterfort/te0058.h'
    include 'asterfort/te0059.h'
    include 'asterfort/te0060.h'
    include 'asterfort/te0061.h'
    include 'asterfort/te0062.h'
    include 'asterfort/te0063.h'
    include 'asterfort/te0064.h'
    include 'asterfort/te0065.h'
    include 'asterfort/te0066.h'
    include 'asterfort/te0067.h'
    include 'asterfort/te0068.h'
    include 'asterfort/te0069.h'
    include 'asterfort/te0070.h'
    include 'asterfort/te0071.h'
    include 'asterfort/te0072.h'
    include 'asterfort/te0073.h'
    include 'asterfort/te0074.h'
    include 'asterfort/te0075.h'
    include 'asterfort/te0076.h'
    include 'asterfort/te0077.h'
    include 'asterfort/te0078.h'
    include 'asterfort/te0079.h'
    include 'asterfort/te0080.h'
    include 'asterfort/te0081.h'
    include 'asterfort/te0082.h'
    include 'asterfort/te0083.h'
    include 'asterfort/te0084.h'
    include 'asterfort/te0085.h'
    include 'asterfort/te0086.h'
    include 'asterfort/te0087.h'
    include 'asterfort/te0088.h'
    include 'asterfort/te0089.h'
    include 'asterfort/te0090.h'
    include 'asterfort/te0091.h'
    include 'asterfort/te0092.h'
    include 'asterfort/te0093.h'
    include 'asterfort/te0094.h'
    include 'asterfort/te0095.h'
    include 'asterfort/te0096.h'
    include 'asterfort/te0097.h'
    include 'asterfort/te0098.h'
    include 'asterfort/te0099.h'
    include 'asterfort/te0100.h'
    include 'asterfort/te0101.h'
    include 'asterfort/te0102.h'
    include 'asterfort/te0103.h'
    include 'asterfort/te0104.h'
    include 'asterfort/te0105.h'
    include 'asterfort/te0106.h'
    include 'asterfort/te0107.h'
    include 'asterfort/te0108.h'
    include 'asterfort/te0109.h'
    include 'asterfort/te0110.h'
    include 'asterfort/te0111.h'
    include 'asterfort/te0112.h'
    include 'asterfort/te0113.h'
    include 'asterfort/te0114.h'
    include 'asterfort/te0115.h'
    include 'asterfort/te0116.h'
    include 'asterfort/te0117.h'
    include 'asterfort/te0118.h'
    include 'asterfort/te0119.h'
    include 'asterfort/te0120.h'
    include 'asterfort/te0121.h'
    include 'asterfort/te0122.h'
    include 'asterfort/te0123.h'
    include 'asterfort/te0124.h'
    include 'asterfort/te0125.h'
    include 'asterfort/te0126.h'
    include 'asterfort/te0127.h'
    include 'asterfort/te0128.h'
    include 'asterfort/te0129.h'
    include 'asterfort/te0130.h'
    include 'asterfort/te0131.h'
    include 'asterfort/te0132.h'
    include 'asterfort/te0133.h'
    include 'asterfort/te0134.h'
    include 'asterfort/te0135.h'
    include 'asterfort/te0136.h'
    include 'asterfort/te0137.h'
    include 'asterfort/te0138.h'
    include 'asterfort/te0139.h'
    include 'asterfort/te0140.h'
    include 'asterfort/te0141.h'
    include 'asterfort/te0142.h'
    include 'asterfort/te0143.h'
    include 'asterfort/te0144.h'
    include 'asterfort/te0145.h'
    include 'asterfort/te0146.h'
    include 'asterfort/te0147.h'
    include 'asterfort/te0148.h'
    include 'asterfort/te0149.h'
    include 'asterfort/te0150.h'
    include 'asterfort/te0151.h'
    include 'asterfort/te0152.h'
    include 'asterfort/te0153.h'
    include 'asterfort/te0154.h'
    include 'asterfort/te0155.h'
    include 'asterfort/te0156.h'
    include 'asterfort/te0157.h'
    include 'asterfort/te0158.h'
    include 'asterfort/te0159.h'
    include 'asterfort/te0160.h'
    include 'asterfort/te0161.h'
    include 'asterfort/te0162.h'
    include 'asterfort/te0163.h'
    include 'asterfort/te0164.h'
    include 'asterfort/te0165.h'
    include 'asterfort/te0166.h'
    include 'asterfort/te0167.h'
    include 'asterfort/te0168.h'
    include 'asterfort/te0169.h'
    include 'asterfort/te0170.h'
    include 'asterfort/te0171.h'
    include 'asterfort/te0172.h'
    include 'asterfort/te0173.h'
    include 'asterfort/te0174.h'
    include 'asterfort/te0175.h'
    include 'asterfort/te0176.h'
    include 'asterfort/te0177.h'
    include 'asterfort/te0178.h'
    include 'asterfort/te0179.h'
    include 'asterfort/te0180.h'
    include 'asterfort/te0181.h'
    include 'asterfort/te0182.h'
    include 'asterfort/te0183.h'
    include 'asterfort/te0184.h'
    include 'asterfort/te0185.h'
    include 'asterfort/te0186.h'
    include 'asterfort/te0187.h'
    include 'asterfort/te0188.h'
    include 'asterfort/te0189.h'
    include 'asterfort/te0190.h'
    include 'asterfort/te0191.h'
    include 'asterfort/te0192.h'
    include 'asterfort/te0193.h'
    include 'asterfort/te0194.h'
    include 'asterfort/te0195.h'
    include 'asterfort/te0196.h'
    include 'asterfort/te0197.h'
    include 'asterfort/te0198.h'
    include 'asterfort/te0199.h'
    include 'asterfort/te0200.h'
    include 'asterfort/te0201.h'
    include 'asterfort/te0202.h'
    include 'asterfort/te0203.h'
    include 'asterfort/te0204.h'
    include 'asterfort/te0205.h'
    include 'asterfort/te0206.h'
    include 'asterfort/te0207.h'
    include 'asterfort/te0208.h'
    include 'asterfort/te0209.h'
    include 'asterfort/te0210.h'
    include 'asterfort/te0211.h'
    include 'asterfort/te0212.h'
    include 'asterfort/te0213.h'
    include 'asterfort/te0214.h'
    include 'asterfort/te0215.h'
    include 'asterfort/te0216.h'
    include 'asterfort/te0217.h'
    include 'asterfort/te0218.h'
    include 'asterfort/te0219.h'
    include 'asterfort/te0220.h'
    include 'asterfort/te0221.h'
    include 'asterfort/te0222.h'
    include 'asterfort/te0223.h'
    include 'asterfort/te0224.h'
    include 'asterfort/te0225.h'
    include 'asterfort/te0226.h'
    include 'asterfort/te0227.h'
    include 'asterfort/te0228.h'
    include 'asterfort/te0229.h'
    include 'asterfort/te0230.h'
    include 'asterfort/te0231.h'
    include 'asterfort/te0232.h'
    include 'asterfort/te0233.h'
    include 'asterfort/te0234.h'
    include 'asterfort/te0235.h'
    include 'asterfort/te0236.h'
    include 'asterfort/te0237.h'
    include 'asterfort/te0238.h'
    include 'asterfort/te0239.h'
    include 'asterfort/te0240.h'
    include 'asterfort/te0241.h'
    include 'asterfort/te0242.h'
    include 'asterfort/te0243.h'
    include 'asterfort/te0244.h'
    include 'asterfort/te0245.h'
    include 'asterfort/te0246.h'
    include 'asterfort/te0247.h'
    include 'asterfort/te0248.h'
    include 'asterfort/te0249.h'
    include 'asterfort/te0250.h'
    include 'asterfort/te0251.h'
    include 'asterfort/te0252.h'
    include 'asterfort/te0253.h'
    include 'asterfort/te0254.h'
    include 'asterfort/te0255.h'
    include 'asterfort/te0256.h'
    include 'asterfort/te0257.h'
    include 'asterfort/te0258.h'
    include 'asterfort/te0259.h'
    include 'asterfort/te0260.h'
    include 'asterfort/te0261.h'
    include 'asterfort/te0262.h'
    include 'asterfort/te0263.h'
    include 'asterfort/te0264.h'
    include 'asterfort/te0265.h'
    include 'asterfort/te0266.h'
    include 'asterfort/te0267.h'
    include 'asterfort/te0268.h'
    include 'asterfort/te0269.h'
    include 'asterfort/te0270.h'
    include 'asterfort/te0271.h'
    include 'asterfort/te0272.h'
    include 'asterfort/te0273.h'
    include 'asterfort/te0274.h'
    include 'asterfort/te0275.h'
    include 'asterfort/te0276.h'
    include 'asterfort/te0277.h'
    include 'asterfort/te0278.h'
    include 'asterfort/te0279.h'
    include 'asterfort/te0280.h'
    include 'asterfort/te0281.h'
    include 'asterfort/te0282.h'
    include 'asterfort/te0283.h'
    include 'asterfort/te0284.h'
    include 'asterfort/te0285.h'
    include 'asterfort/te0286.h'
    include 'asterfort/te0287.h'
    include 'asterfort/te0288.h'
    include 'asterfort/te0289.h'
    include 'asterfort/te0290.h'
    include 'asterfort/te0291.h'
    include 'asterfort/te0292.h'
    include 'asterfort/te0293.h'
    include 'asterfort/te0294.h'
    include 'asterfort/te0295.h'
    include 'asterfort/te0296.h'
    include 'asterfort/te0297.h'
    include 'asterfort/te0298.h'
    include 'asterfort/te0299.h'
    include 'asterfort/te0300.h'
    include 'asterfort/te0301.h'
    include 'asterfort/te0302.h'
    include 'asterfort/te0303.h'
    include 'asterfort/te0304.h'
    include 'asterfort/te0305.h'
    include 'asterfort/te0306.h'
    include 'asterfort/te0307.h'
    include 'asterfort/te0308.h'
    include 'asterfort/te0309.h'
    include 'asterfort/te0310.h'
    include 'asterfort/te0311.h'
    include 'asterfort/te0312.h'
    include 'asterfort/te0313.h'
    include 'asterfort/te0314.h'
    include 'asterfort/te0315.h'
    include 'asterfort/te0316.h'
    include 'asterfort/te0317.h'
    include 'asterfort/te0318.h'
    include 'asterfort/te0319.h'
    include 'asterfort/te0320.h'
    include 'asterfort/te0321.h'
    include 'asterfort/te0322.h'
    include 'asterfort/te0323.h'
    include 'asterfort/te0324.h'
    include 'asterfort/te0325.h'
    include 'asterfort/te0326.h'
    include 'asterfort/te0327.h'
    include 'asterfort/te0328.h'
    include 'asterfort/te0329.h'
    include 'asterfort/te0330.h'
    include 'asterfort/te0331.h'
    include 'asterfort/te0332.h'
    include 'asterfort/te0333.h'
    include 'asterfort/te0334.h'
    include 'asterfort/te0335.h'
    include 'asterfort/te0336.h'
    include 'asterfort/te0337.h'
    include 'asterfort/te0338.h'
    include 'asterfort/te0339.h'
    include 'asterfort/te0340.h'
    include 'asterfort/te0341.h'
    include 'asterfort/te0342.h'
    include 'asterfort/te0343.h'
    include 'asterfort/te0344.h'
    include 'asterfort/te0345.h'
    include 'asterfort/te0346.h'
    include 'asterfort/te0347.h'
    include 'asterfort/te0348.h'
    include 'asterfort/te0349.h'
    include 'asterfort/te0350.h'
    include 'asterfort/te0351.h'
    include 'asterfort/te0352.h'
    include 'asterfort/te0353.h'
    include 'asterfort/te0354.h'
    include 'asterfort/te0355.h'
    include 'asterfort/te0356.h'
    include 'asterfort/te0357.h'
    include 'asterfort/te0358.h'
    include 'asterfort/te0359.h'
    include 'asterfort/te0360.h'
    include 'asterfort/te0361.h'
    include 'asterfort/te0362.h'
    include 'asterfort/te0363.h'
    include 'asterfort/te0364.h'
    include 'asterfort/te0365.h'
    include 'asterfort/te0366.h'
    include 'asterfort/te0367.h'
    include 'asterfort/te0368.h'
    include 'asterfort/te0369.h'
    include 'asterfort/te0370.h'
    include 'asterfort/te0371.h'
    include 'asterfort/te0372.h'
    include 'asterfort/te0373.h'
    include 'asterfort/te0374.h'
    include 'asterfort/te0375.h'
    include 'asterfort/te0376.h'
    include 'asterfort/te0377.h'
    include 'asterfort/te0378.h'
    include 'asterfort/te0379.h'
    include 'asterfort/te0380.h'
    include 'asterfort/te0381.h'
    include 'asterfort/te0382.h'
    include 'asterfort/te0383.h'
    include 'asterfort/te0384.h'
    include 'asterfort/te0385.h'
    include 'asterfort/te0386.h'
    include 'asterfort/te0387.h'
    include 'asterfort/te0388.h'
    include 'asterfort/te0389.h'
    include 'asterfort/te0390.h'
    include 'asterfort/te0391.h'
    include 'asterfort/te0392.h'
    include 'asterfort/te0393.h'
    include 'asterfort/te0394.h'
    include 'asterfort/te0395.h'
    include 'asterfort/te0396.h'
    include 'asterfort/te0397.h'
    include 'asterfort/te0398.h'
    include 'asterfort/te0399.h'
    include 'asterfort/te0400.h'
    include 'asterfort/te0401.h'
    include 'asterfort/te0402.h'
    include 'asterfort/te0403.h'
    include 'asterfort/te0404.h'
    include 'asterfort/te0405.h'
    include 'asterfort/te0406.h'
    include 'asterfort/te0407.h'
    include 'asterfort/te0408.h'
    include 'asterfort/te0409.h'
    include 'asterfort/te0410.h'
    include 'asterfort/te0411.h'
    include 'asterfort/te0412.h'
    include 'asterfort/te0413.h'
    include 'asterfort/te0414.h'
    include 'asterfort/te0415.h'
    include 'asterfort/te0416.h'
    include 'asterfort/te0417.h'
    include 'asterfort/te0418.h'
    include 'asterfort/te0419.h'
    include 'asterfort/te0420.h'
    include 'asterfort/te0421.h'
    include 'asterfort/te0422.h'
    include 'asterfort/te0423.h'
    include 'asterfort/te0424.h'
    include 'asterfort/te0425.h'
    include 'asterfort/te0426.h'
    include 'asterfort/te0427.h'
    include 'asterfort/te0428.h'
    include 'asterfort/te0429.h'
    include 'asterfort/te0430.h'
    include 'asterfort/te0431.h'
    include 'asterfort/te0432.h'
    include 'asterfort/te0433.h'
    include 'asterfort/te0434.h'
    include 'asterfort/te0435.h'
    include 'asterfort/te0436.h'
    include 'asterfort/te0437.h'
    include 'asterfort/te0438.h'
    include 'asterfort/te0439.h'
    include 'asterfort/te0440.h'
    include 'asterfort/te0441.h'
    include 'asterfort/te0442.h'
    include 'asterfort/te0443.h'
    include 'asterfort/te0444.h'
    include 'asterfort/te0445.h'
    include 'asterfort/te0446.h'
    include 'asterfort/te0447.h'
    include 'asterfort/te0448.h'
    include 'asterfort/te0449.h'
    include 'asterfort/te0450.h'
    include 'asterfort/te0451.h'
    include 'asterfort/te0452.h'
    include 'asterfort/te0453.h'
    include 'asterfort/te0454.h'
    include 'asterfort/te0455.h'
    include 'asterfort/te0456.h'
    include 'asterfort/te0457.h'
    include 'asterfort/te0458.h'
    include 'asterfort/te0459.h'
    include 'asterfort/te0460.h'
    include 'asterfort/te0461.h'
    include 'asterfort/te0462.h'
    include 'asterfort/te0463.h'
    include 'asterfort/te0464.h'
    include 'asterfort/te0465.h'
    include 'asterfort/te0466.h'
    include 'asterfort/te0467.h'
    include 'asterfort/te0468.h'
    include 'asterfort/te0469.h'
    include 'asterfort/te0470.h'
    include 'asterfort/te0471.h'
    include 'asterfort/te0472.h'
    include 'asterfort/te0473.h'
    include 'asterfort/te0474.h'
    include 'asterfort/te0475.h'
    include 'asterfort/te0476.h'
    include 'asterfort/te0477.h'
    include 'asterfort/te0478.h'
    include 'asterfort/te0479.h'
    include 'asterfort/te0480.h'
    include 'asterfort/te0481.h'
    include 'asterfort/te0482.h'
    include 'asterfort/te0483.h'
    include 'asterfort/te0484.h'
    include 'asterfort/te0485.h'
    include 'asterfort/te0486.h'
    include 'asterfort/te0487.h'
    include 'asterfort/te0488.h'
    include 'asterfort/te0489.h'
    include 'asterfort/te0490.h'
    include 'asterfort/te0491.h'
    include 'asterfort/te0492.h'
    include 'asterfort/te0493.h'
    include 'asterfort/te0494.h'
    include 'asterfort/te0495.h'
    include 'asterfort/te0496.h'
    include 'asterfort/te0497.h'
    include 'asterfort/te0498.h'
    include 'asterfort/te0499.h'
    include 'asterfort/te0500.h'
    include 'asterfort/te0501.h'
    include 'asterfort/te0502.h'
    include 'asterfort/te0503.h'
    include 'asterfort/te0504.h'
    include 'asterfort/te0505.h'
    include 'asterfort/te0506.h'
    include 'asterfort/te0507.h'
    include 'asterfort/te0508.h'
    include 'asterfort/te0509.h'
    include 'asterfort/te0510.h'
    include 'asterfort/te0511.h'
    include 'asterfort/te0512.h'
    include 'asterfort/te0513.h'
    include 'asterfort/te0514.h'
    include 'asterfort/te0515.h'
    include 'asterfort/te0516.h'
    include 'asterfort/te0517.h'
    include 'asterfort/te0518.h'
    include 'asterfort/te0519.h'
    include 'asterfort/te0520.h'
    include 'asterfort/te0521.h'
    include 'asterfort/te0522.h'
    include 'asterfort/te0523.h'
    include 'asterfort/te0524.h'
    include 'asterfort/te0525.h'
    include 'asterfort/te0526.h'
    include 'asterfort/te0527.h'
    include 'asterfort/te0528.h'
    include 'asterfort/te0529.h'
    include 'asterfort/te0530.h'
    include 'asterfort/te0531.h'
    include 'asterfort/te0532.h'
    include 'asterfort/te0533.h'
    include 'asterfort/te0534.h'
    include 'asterfort/te0535.h'
    include 'asterfort/te0536.h'
    include 'asterfort/te0537.h'
    include 'asterfort/te0538.h'
    include 'asterfort/te0539.h'
    include 'asterfort/te0540.h'
    include 'asterfort/te0541.h'
    include 'asterfort/te0542.h'
    include 'asterfort/te0543.h'
    include 'asterfort/te0544.h'
    include 'asterfort/te0545.h'
    include 'asterfort/te0546.h'
    include 'asterfort/te0547.h'
    include 'asterfort/te0548.h'
    include 'asterfort/te0549.h'
    include 'asterfort/te0550.h'
    include 'asterfort/te0551.h'
    include 'asterfort/te0552.h'
    include 'asterfort/te0553.h'
    include 'asterfort/te0554.h'
    include 'asterfort/te0555.h'
    include 'asterfort/te0556.h'
    include 'asterfort/te0557.h'
    include 'asterfort/te0558.h'
    include 'asterfort/te0559.h'
    include 'asterfort/te0560.h'
    include 'asterfort/te0561.h'
    include 'asterfort/te0562.h'
    include 'asterfort/te0563.h'
    include 'asterfort/te0564.h'
    include 'asterfort/te0565.h'
    include 'asterfort/te0566.h'
    include 'asterfort/te0567.h'
    include 'asterfort/te0568.h'
    include 'asterfort/te0569.h'
    include 'asterfort/te0570.h'
    include 'asterfort/te0571.h'
    include 'asterfort/te0572.h'
    include 'asterfort/te0573.h'
    include 'asterfort/te0574.h'
    include 'asterfort/te0575.h'
    include 'asterfort/te0576.h'
    include 'asterfort/te0577.h'
    include 'asterfort/te0578.h'
    include 'asterfort/te0579.h'
    include 'asterfort/te0580.h'
    include 'asterfort/te0581.h'
    include 'asterfort/te0582.h'
    include 'asterfort/te0583.h'
    include 'asterfort/te0584.h'
    include 'asterfort/te0585.h'
    include 'asterfort/te0586.h'
    include 'asterfort/te0587.h'
    include 'asterfort/te0588.h'
    include 'asterfort/te0589.h'
    include 'asterfort/te0590.h'
    include 'asterfort/te0591.h'
    include 'asterfort/te0592.h'
    include 'asterfort/te0593.h'
    include 'asterfort/te0594.h'
    include 'asterfort/te0595.h'
    include 'asterfort/te0596.h'
    include 'asterfort/te0597.h'
    include 'asterfort/te0598.h'
    include 'asterfort/te0599.h'
    include 'asterfort/te0600.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/uttcpu.h'
    integer :: numc, opt, te
! ----------------------------------------------------------------------
!     ENTREES:
!      NUMC  :  NUMERO DU CALCUL A LANCER
!      OPT   :  OPTION_SIMPLE
!      TE    :  TYPE_ELEMENT
!
!     SORTIES:
!      ON LANCE LE BON CA_NUMC(OPT,TE)
!
! ----------------------------------------------------------------------
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    integer :: ianoop, ianote, nbobtr, iaobtr, nbobmx
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    integer :: iel
    common /caii08/iel
    integer :: caindz(512), capoiz
    common /caii12/caindz,capoiz
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: iret, jparal, numc2, numc3
    logical :: lparal
    character(len=16) :: nomte, nomopt
    character(len=8) :: k8bid
! DEB-------------------------------------------------------------------
    call uttcpu('CPU.CALC.3', 'DEBUT', ' ')
!
!     PARALLELE OR NOT ?
!     --------------------
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal =.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal =.false.
    endif
!
    nomte = zk16(ianote-1+te)
    nomopt = zk16(ianoop-1+opt)
!
    numc2= (numc-1)/100
    numc3= numc-numc2*100
!
    numc2= numc2+1
    goto (9991,9992,9993,9994,9995,9996) numc2
    goto 9998
!
!
!     -- TE0001 --> TE0100
!     --------------------
9991  continue
    do 1, iel=1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) goto 1
    endif
    capoiz=0
    goto (101,102,103,104,105,106,107,108,109,110,111,112,113,114,&
        115,116,117,118,119,120,121,122,123,124,125,126,127,128,&
        129,130,131,132,133,134,135,136,137,138,139,140,141,142,&
        143,144,145,146,147,148,149,150,151,152,153,154,155,156,&
        157,158,159,160,161,162,163,164,165,166,167,168,169,170,&
        171,172,173,174,175,176,177,178,179,180,181,182,183,184,&
        185,186,187,188,189,190,191,192,193,194,195,196,197,198,&
        199,200) numc3
101  continue
    call te0001(nomopt, nomte)
    goto 1
102  continue
    call te0002(nomopt, nomte)
    goto 1
103  continue
    call te0003(nomopt, nomte)
    goto 1
104  continue
    call te0004(nomopt, nomte)
    goto 1
105  continue
    call te0005(nomopt, nomte)
    goto 1
106  continue
    call te0006(nomopt, nomte)
    goto 1
107  continue
    call te0007(nomopt, nomte)
    goto 1
108  continue
    call te0008(nomopt, nomte)
    goto 1
109  continue
    call te0009(nomopt, nomte)
    goto 1
110  continue
    call te0010(nomopt, nomte)
    goto 1
111  continue
    call te0011(nomopt, nomte)
    goto 1
112  continue
    call te0012(nomopt, nomte)
    goto 1
113  continue
    call te0013(nomopt, nomte)
    goto 1
114  continue
    call te0014(nomopt, nomte)
    goto 1
115  continue
    call te0015(nomopt, nomte)
    goto 1
116  continue
    call te0016(nomopt, nomte)
    goto 1
117  continue
    call te0017(nomopt, nomte)
    goto 1
118  continue
    call te0018(nomopt, nomte)
    goto 1
119  continue
    call te0019(nomopt, nomte)
    goto 1
120  continue
    call te0020(nomopt, nomte)
    goto 1
121  continue
    call te0021(nomopt, nomte)
    goto 1
122  continue
    call te0022(nomopt, nomte)
    goto 1
123  continue
    call te0023(nomopt, nomte)
    goto 1
124  continue
    call te0024(nomopt, nomte)
    goto 1
125  continue
    call te0025(nomopt, nomte)
    goto 1
126  continue
    call te0026(nomopt, nomte)
    goto 1
127  continue
    call te0027(nomopt, nomte)
    goto 1
128  continue
    call te0028(nomopt, nomte)
    goto 1
129  continue
    call te0029(nomopt, nomte)
    goto 1
130  continue
    call te0030(nomopt, nomte)
    goto 1
131  continue
    call te0031(nomopt, nomte)
    goto 1
132  continue
    call te0032(nomopt, nomte)
    goto 1
133  continue
    call te0033(nomopt, nomte)
    goto 1
134  continue
    call te0034(nomopt, nomte)
    goto 1
135  continue
    call te0035(nomopt, nomte)
    goto 1
136  continue
    call te0036(nomopt, nomte)
    goto 1
137  continue
    call te0037(nomopt, nomte)
    goto 1
138  continue
    call te0038(nomopt, nomte)
    goto 1
139  continue
    call te0039(nomopt, nomte)
    goto 1
140  continue
    call te0040(nomopt, nomte)
    goto 1
141  continue
    call te0041(nomopt, nomte)
    goto 1
142  continue
    call te0042(nomopt, nomte)
    goto 1
143  continue
    call te0043(nomopt, nomte)
    goto 1
144  continue
    call te0044(nomopt, nomte)
    goto 1
145  continue
    call te0045(nomopt, nomte)
    goto 1
146  continue
    call te0046(nomopt, nomte)
    goto 1
147  continue
    call te0047(nomopt, nomte)
    goto 1
148  continue
    call te0048(nomopt, nomte)
    goto 1
149  continue
    call te0049(nomopt, nomte)
    goto 1
150  continue
    call te0050(nomopt, nomte)
    goto 1
151  continue
    call te0051(nomopt, nomte)
    goto 1
152  continue
    call te0052(nomopt, nomte)
    goto 1
153  continue
    call te0053(nomopt, nomte)
    goto 1
154  continue
    call te0054(nomopt, nomte)
    goto 1
155  continue
    call te0055(nomopt, nomte)
    goto 1
156  continue
    call te0056(nomopt, nomte)
    goto 1
157  continue
    call te0057(nomopt, nomte)
    goto 1
158  continue
    call te0058(nomopt, nomte)
    goto 1
159  continue
    call te0059(nomopt, nomte)
    goto 1
160  continue
    call te0060(nomopt, nomte)
    goto 1
161  continue
    call te0061(nomopt, nomte)
    goto 1
162  continue
    call te0062(nomopt, nomte)
    goto 1
163  continue
    call te0063(nomopt, nomte)
    goto 1
164  continue
    call te0064(nomopt, nomte)
    goto 1
165  continue
    call te0065(nomopt, nomte)
    goto 1
166  continue
    call te0066(nomopt, nomte)
    goto 1
167  continue
    call te0067(nomopt, nomte)
    goto 1
168  continue
    call te0068(nomopt, nomte)
    goto 1
169  continue
    call te0069(nomopt, nomte)
    goto 1
170  continue
    call te0070(nomopt, nomte)
    goto 1
171  continue
    call te0071(nomopt, nomte)
    goto 1
172  continue
    call te0072(nomopt, nomte)
    goto 1
173  continue
    call te0073(nomopt, nomte)
    goto 1
174  continue
    call te0074(nomopt, nomte)
    goto 1
175  continue
    call te0075(nomopt, nomte)
    goto 1
176  continue
    call te0076(nomopt, nomte)
    goto 1
177  continue
    call te0077(nomopt, nomte)
    goto 1
178  continue
    call te0078(nomopt, nomte)
    goto 1
179  continue
    call te0079(nomopt, nomte)
    goto 1
180  continue
    call te0080(nomopt, nomte)
    goto 1
181  continue
    call te0081(nomopt, nomte)
    goto 1
182  continue
    call te0082(nomopt, nomte)
    goto 1
183  continue
    call te0083(nomopt, nomte)
    goto 1
184  continue
    call te0084(nomopt, nomte)
    goto 1
185  continue
    call te0085(nomopt, nomte)
    goto 1
186  continue
    call te0086(nomopt, nomte)
    goto 1
187  continue
    call te0087(nomopt, nomte)
    goto 1
188  continue
    call te0088(nomopt, nomte)
    goto 1
189  continue
    call te0089(nomopt, nomte)
    goto 1
190  continue
    call te0090(nomopt, nomte)
    goto 1
191  continue
    call te0091(nomopt, nomte)
    goto 1
192  continue
    call te0092(nomopt, nomte)
    goto 1
193  continue
    call te0093(nomopt, nomte)
    goto 1
194  continue
    call te0094(nomopt, nomte)
    goto 1
195  continue
    call te0095(nomopt, nomte)
    goto 1
196  continue
    call te0096(nomopt, nomte)
    goto 1
197  continue
    call te0097(nomopt, nomte)
    goto 1
198  continue
    call te0098(nomopt, nomte)
    goto 1
199  continue
    call te0099(nomopt, nomte)
    goto 1
200  continue
    call te0100(nomopt, nomte)
!
    goto 1
    1 end do
    goto 9999
!
!     -- TE0101 --> TE0200
!     --------------------
9992  continue
    do 2, iel=1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) goto 2
    endif
    capoiz=0
    goto (201,202,203,204,205,206,207,208,209,210,211,212,213,214,&
        215,216,217,218,219,220,221,222,223,224,225,226,227,228,&
        229,230,231,232,233,234,235,236,237,238,239,240,241,242,&
        243,244,245,246,247,248,249,250,251,252,253,254,255,256,&
        257,258,259,260,261,262,263,264,265,266,267,268,269,270,&
        271,272,273,274,275,276,277,278,279,280,281,282,283,284,&
        285,286,287,288,289,290,291,292,293,294,295,296,297,298,&
        299,300) numc3
201  continue
    call te0101(nomopt, nomte)
    goto 2
202  continue
    call te0102(nomopt, nomte)
    goto 2
203  continue
    call te0103(nomopt, nomte)
    goto 2
204  continue
    call te0104(nomopt, nomte)
    goto 2
205  continue
    call te0105(nomopt, nomte)
    goto 2
206  continue
    call te0106(nomopt, nomte)
    goto 2
207  continue
    call te0107(nomopt, nomte)
    goto 2
208  continue
    call te0108(nomopt, nomte)
    goto 2
209  continue
    call te0109(nomopt, nomte)
    goto 2
210  continue
    call te0110(nomopt, nomte)
    goto 2
211  continue
    call te0111(nomopt, nomte)
    goto 2
212  continue
    call te0112(nomopt, nomte)
    goto 2
213  continue
    call te0113(nomopt, nomte)
    goto 2
214  continue
    call te0114(nomopt, nomte)
    goto 2
215  continue
    call te0115(nomopt, nomte)
    goto 2
216  continue
    call te0116(nomopt, nomte)
    goto 2
217  continue
    call te0117(nomopt, nomte)
    goto 2
218  continue
    call te0118(nomopt, nomte)
    goto 2
219  continue
    call te0119(nomopt, nomte)
    goto 2
220  continue
    call te0120(nomopt, nomte)
    goto 2
221  continue
    call te0121(nomopt, nomte)
    goto 2
222  continue
    call te0122(nomopt, nomte)
    goto 2
223  continue
    call te0123(nomopt, nomte)
    goto 2
224  continue
    call te0124(nomopt, nomte)
    goto 2
225  continue
    call te0125(nomopt, nomte)
    goto 2
226  continue
    call te0126(nomopt, nomte)
    goto 2
227  continue
    call te0127(nomopt, nomte)
    goto 2
228  continue
    call te0128(nomopt, nomte)
    goto 2
229  continue
    call te0129(nomopt, nomte)
    goto 2
230  continue
    call te0130(nomopt, nomte)
    goto 2
231  continue
    call te0131(nomopt, nomte)
    goto 2
232  continue
    call te0132(nomopt, nomte)
    goto 2
233  continue
    call te0133(nomopt, nomte)
    goto 2
234  continue
    call te0134(nomopt, nomte)
    goto 2
235  continue
    call te0135(nomopt, nomte)
    goto 2
236  continue
    call te0136(nomopt, nomte)
    goto 2
237  continue
    call te0137(nomopt, nomte)
    goto 2
238  continue
    call te0138(nomopt, nomte)
    goto 2
239  continue
    call te0139(nomopt, nomte)
    goto 2
240  continue
    call te0140(nomopt, nomte)
    goto 2
241  continue
    call te0141(nomopt, nomte)
    goto 2
242  continue
    call te0142(nomopt, nomte)
    goto 2
243  continue
    call te0143(nomopt, nomte)
    goto 2
244  continue
    call te0144(nomopt, nomte)
    goto 2
245  continue
    call te0145(nomopt, nomte)
    goto 2
246  continue
    call te0146(nomopt, nomte)
    goto 2
247  continue
    call te0147(nomopt, nomte)
    goto 2
248  continue
    call te0148(nomopt, nomte)
    goto 2
249  continue
    call te0149(nomopt, nomte)
    goto 2
250  continue
    call te0150(nomopt, nomte)
    goto 2
251  continue
    call te0151(nomopt, nomte)
    goto 2
252  continue
    call te0152(nomopt, nomte)
    goto 2
253  continue
    call te0153(nomopt, nomte)
    goto 2
254  continue
    call te0154(nomopt, nomte)
    goto 2
255  continue
    call te0155(nomopt, nomte)
    goto 2
256  continue
    call te0156(nomopt, nomte)
    goto 2
257  continue
    call te0157(nomopt, nomte)
    goto 2
258  continue
    call te0158(nomopt, nomte)
    goto 2
259  continue
    call te0159(nomopt, nomte)
    goto 2
260  continue
    call te0160(nomopt, nomte)
    goto 2
261  continue
    call te0161(nomopt, nomte)
    goto 2
262  continue
    call te0162(nomopt, nomte)
    goto 2
263  continue
    call te0163(nomopt, nomte)
    goto 2
264  continue
    call te0164(nomopt, nomte)
    goto 2
265  continue
    call te0165(nomopt, nomte)
    goto 2
266  continue
    call te0166(nomopt, nomte)
    goto 2
267  continue
    call te0167(nomopt, nomte)
    goto 2
268  continue
    call te0168(nomopt, nomte)
    goto 2
269  continue
    call te0169(nomopt, nomte)
    goto 2
270  continue
    call te0170(nomopt, nomte)
    goto 2
271  continue
    call te0171(nomopt, nomte)
    goto 2
272  continue
    call te0172(nomopt, nomte)
    goto 2
273  continue
    call te0173(nomopt, nomte)
    goto 2
274  continue
    call te0174(nomopt, nomte)
    goto 2
275  continue
    call te0175(nomopt, nomte)
    goto 2
276  continue
    call te0176(nomopt, nomte)
    goto 2
277  continue
    call te0177(nomopt, nomte)
    goto 2
278  continue
    call te0178(nomopt, nomte)
    goto 2
279  continue
    call te0179(nomopt, nomte)
    goto 2
280  continue
    call te0180(nomopt, nomte)
    goto 2
281  continue
    call te0181(nomopt, nomte)
    goto 2
282  continue
    call te0182(nomopt, nomte)
    goto 2
283  continue
    call te0183(nomopt, nomte)
    goto 2
284  continue
    call te0184(nomopt, nomte)
    goto 2
285  continue
    call te0185(nomopt, nomte)
    goto 2
286  continue
    call te0186(nomopt, nomte)
    goto 2
287  continue
    call te0187(nomopt, nomte)
    goto 2
288  continue
    call te0188(nomopt, nomte)
    goto 2
289  continue
    call te0189(nomopt, nomte)
    goto 2
290  continue
    call te0190(nomopt, nomte)
    goto 2
291  continue
    call te0191(nomopt, nomte)
    goto 2
292  continue
    call te0192(nomopt, nomte)
    goto 2
293  continue
    call te0193(nomopt, nomte)
    goto 2
294  continue
    call te0194(nomopt, nomte)
    goto 2
295  continue
    call te0195(nomopt, nomte)
    goto 2
296  continue
    call te0196(nomopt, nomte)
    goto 2
297  continue
    call te0197(nomopt, nomte)
    goto 2
298  continue
    call te0198(nomopt, nomte)
    goto 2
299  continue
    call te0199(nomopt, nomte)
    goto 2
300  continue
    call te0200(nomopt, nomte)
    goto 2
    2 end do
    goto 9999
!
!     -- TE0201 --> TE0300
!     --------------------
9993  continue
    do 3, iel=1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) goto 3
    endif
    capoiz=0
    goto (301,302,303,304,305,306,307,308,309,310,311,312,313,314,&
        315,316,317,318,319,320,321,322,323,324,325,326,327,328,&
        329,330,331,332,333,334,335,336,337,338,339,340,341,342,&
        343,344,345,346,347,348,349,350,351,352,353,354,355,356,&
        357,358,359,360,361,362,363,364,365,366,367,368,369,370,&
        371,372,373,374,375,376,377,378,379,380,381,382,383,384,&
        385,386,387,388,389,390,391,392,393,394,395,396,397,398,&
        399,400) numc3
301  continue
    call te0201(nomopt, nomte)
    goto 3
302  continue
    call te0202(nomopt, nomte)
    goto 3
303  continue
    call te0203(nomopt, nomte)
    goto 3
304  continue
    call te0204(nomopt, nomte)
    goto 3
305  continue
    call te0205(nomopt, nomte)
    goto 3
306  continue
    call te0206(nomopt, nomte)
    goto 3
307  continue
    call te0207(nomopt, nomte)
    goto 3
308  continue
    call te0208(nomopt, nomte)
    goto 3
309  continue
    call te0209(nomopt, nomte)
    goto 3
310  continue
    call te0210(nomopt, nomte)
    goto 3
311  continue
    call te0211(nomopt, nomte)
    goto 3
312  continue
    call te0212(nomopt, nomte)
    goto 3
313  continue
    call te0213(nomopt, nomte)
    goto 3
314  continue
    call te0214(nomopt, nomte)
    goto 3
315  continue
    call te0215(nomopt, nomte)
    goto 3
316  continue
    call te0216(nomopt, nomte)
    goto 3
317  continue
    call te0217(nomopt, nomte)
    goto 3
318  continue
    call te0218(nomopt, nomte)
    goto 3
319  continue
    call te0219(nomopt, nomte)
    goto 3
320  continue
    call te0220(nomopt, nomte)
    goto 3
321  continue
    call te0221(nomopt, nomte)
    goto 3
322  continue
    call te0222(nomopt, nomte)
    goto 3
323  continue
    call te0223(nomopt, nomte)
    goto 3
324  continue
    call te0224(nomopt, nomte)
    goto 3
325  continue
    call te0225(nomopt, nomte)
    goto 3
326  continue
    call te0226(nomopt, nomte)
    goto 3
327  continue
    call te0227(nomopt, nomte)
    goto 3
328  continue
    call te0228(nomopt, nomte)
    goto 3
329  continue
    call te0229(nomopt, nomte)
    goto 3
330  continue
    call te0230(nomopt, nomte)
    goto 3
331  continue
    call te0231(nomopt, nomte)
    goto 3
332  continue
    call te0232(nomopt, nomte)
    goto 3
333  continue
    call te0233(nomopt, nomte)
    goto 3
334  continue
    call te0234(nomopt, nomte)
    goto 3
335  continue
    call te0235(nomopt, nomte)
    goto 3
336  continue
    call te0236(nomopt, nomte)
    goto 3
337  continue
    call te0237(nomopt, nomte)
    goto 3
338  continue
    call te0238(nomopt, nomte)
    goto 3
339  continue
    call te0239(nomopt, nomte)
    goto 3
340  continue
    call te0240(nomopt, nomte)
    goto 3
341  continue
    call te0241(nomopt, nomte)
    goto 3
342  continue
    call te0242(nomopt, nomte)
    goto 3
343  continue
    call te0243(nomopt, nomte)
    goto 3
344  continue
    call te0244(nomopt, nomte)
    goto 3
345  continue
    call te0245(nomopt, nomte)
    goto 3
346  continue
    call te0246(nomopt, nomte)
    goto 3
347  continue
    call te0247(nomopt, nomte)
    goto 3
348  continue
    call te0248(nomopt, nomte)
    goto 3
349  continue
    call te0249(nomopt, nomte)
    goto 3
350  continue
    call te0250(nomopt, nomte)
    goto 3
351  continue
    call te0251(nomopt, nomte)
    goto 3
352  continue
    call te0252(nomopt, nomte)
    goto 3
353  continue
    call te0253(nomopt, nomte)
    goto 3
354  continue
    call te0254(nomopt, nomte)
    goto 3
355  continue
    call te0255(nomopt, nomte)
    goto 3
356  continue
    call te0256(nomopt, nomte)
    goto 3
357  continue
    call te0257(nomopt, nomte)
    goto 3
358  continue
    call te0258(nomopt, nomte)
    goto 3
359  continue
    call te0259(nomopt, nomte)
    goto 3
360  continue
    call te0260(nomopt, nomte)
    goto 3
361  continue
    call te0261(nomopt, nomte)
    goto 3
362  continue
    call te0262(nomopt, nomte)
    goto 3
363  continue
    call te0263(nomopt, nomte)
    goto 3
364  continue
    call te0264(nomopt, nomte)
    goto 3
365  continue
    call te0265(nomopt, nomte)
    goto 3
366  continue
    call te0266(nomopt, nomte)
    goto 3
367  continue
    call te0267(nomopt, nomte)
    goto 3
368  continue
    call te0268(nomopt, nomte)
    goto 3
369  continue
    call te0269(nomopt, nomte)
    goto 3
370  continue
    call te0270(nomopt, nomte)
    goto 3
371  continue
    call te0271(nomopt, nomte)
    goto 3
372  continue
    call te0272(nomopt, nomte)
    goto 3
373  continue
    call te0273(nomopt, nomte)
    goto 3
374  continue
    call te0274(nomopt, nomte)
    goto 3
375  continue
    call te0275(nomopt, nomte)
    goto 3
376  continue
    call te0276(nomopt, nomte)
    goto 3
377  continue
    call te0277(nomopt, nomte)
    goto 3
378  continue
    call te0278(nomopt, nomte)
    goto 3
379  continue
    call te0279(nomopt, nomte)
    goto 3
380  continue
    call te0280(nomopt, nomte)
    goto 3
381  continue
    call te0281(nomopt, nomte)
    goto 3
382  continue
    call te0282(nomopt, nomte)
    goto 3
383  continue
    call te0283(nomopt, nomte)
    goto 3
384  continue
    call te0284(nomopt, nomte)
    goto 3
385  continue
    call te0285(nomopt, nomte)
    goto 3
386  continue
    call te0286(nomopt, nomte)
    goto 3
387  continue
    call te0287(nomopt, nomte)
    goto 3
388  continue
    call te0288(nomopt, nomte)
    goto 3
389  continue
    call te0289(nomopt, nomte)
    goto 3
390  continue
    call te0290(nomopt, nomte)
    goto 3
391  continue
    call te0291(nomopt, nomte)
    goto 3
392  continue
    call te0292(nomopt, nomte)
    goto 3
393  continue
    call te0293(nomopt, nomte)
    goto 3
394  continue
    call te0294(nomopt, nomte)
    goto 3
395  continue
    call te0295(nomopt, nomte)
    goto 3
396  continue
    call te0296(nomopt, nomte)
    goto 3
397  continue
    call te0297(nomopt, nomte)
    goto 3
398  continue
    call te0298(nomopt, nomte)
    goto 3
399  continue
    call te0299(nomopt, nomte)
    goto 3
400  continue
    call te0300(nomopt, nomte)
    goto 3
    3 end do
    goto 9999
!
!     -- TE0301 --> TE0400
!     --------------------
9994  continue
    do 4, iel=1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) goto 4
    endif
    capoiz=0
    goto (401,402,403,404,405,406,407,408,409,410,411,412,413,414,&
        415,416,417,418,419,420,421,422,423,424,425,426,427,428,&
        429,430,431,432,433,434,435,436,437,438,439,440,441,442,&
        443,444,445,446,447,448,449,450,451,452,453,454,455,456,&
        457,458,459,460,461,462,463,464,465,466,467,468,469,470,&
        471,472,473,474,475,476,477,478,479,480,481,482,483,484,&
        485,486,487,488,489,490,491,492,493,494,495,496,497,498,&
        499,500) numc3
401  continue
    call te0301(nomopt, nomte)
    goto 4
402  continue
    call te0302(nomopt, nomte)
    goto 4
403  continue
    call te0303(nomopt, nomte)
    goto 4
404  continue
    call te0304(nomopt, nomte)
    goto 4
405  continue
    call te0305(nomopt, nomte)
    goto 4
406  continue
    call te0306(nomopt, nomte)
    goto 4
407  continue
    call te0307(nomopt, nomte)
    goto 4
408  continue
    call te0308(nomopt, nomte)
    goto 4
409  continue
    call te0309(nomopt, nomte)
    goto 4
410  continue
    call te0310(nomopt, nomte)
    goto 4
411  continue
    call te0311(nomopt, nomte)
    goto 4
412  continue
    call te0312(nomopt, nomte)
    goto 4
413  continue
    call te0313(nomopt, nomte)
    goto 4
414  continue
    call te0314(nomopt, nomte)
    goto 4
415  continue
    call te0315(nomopt, nomte)
    goto 4
416  continue
    call te0316(nomopt, nomte)
    goto 4
417  continue
    call te0317(nomopt, nomte)
    goto 4
418  continue
    call te0318(nomopt, nomte)
    goto 4
419  continue
    call te0319(nomopt, nomte)
    goto 4
420  continue
    call te0320(nomopt, nomte)
    goto 4
421  continue
    call te0321(nomopt, nomte)
    goto 4
422  continue
    call te0322(nomopt, nomte)
    goto 4
423  continue
    call te0323(nomopt, nomte)
    goto 4
424  continue
    call te0324(nomopt, nomte)
    goto 4
425  continue
    call te0325(nomopt, nomte)
    goto 4
426  continue
    call te0326(nomopt, nomte)
    goto 4
427  continue
    call te0327(nomopt, nomte)
    goto 4
428  continue
    call te0328(nomopt, nomte)
    goto 4
429  continue
    call te0329(nomopt, nomte)
    goto 4
430  continue
    call te0330(nomopt, nomte)
    goto 4
431  continue
    call te0331(nomopt, nomte)
    goto 4
432  continue
    call te0332(nomopt, nomte)
    goto 4
433  continue
    call te0333(nomopt, nomte)
    goto 4
434  continue
    call te0334(nomopt, nomte)
    goto 4
435  continue
    call te0335(nomopt, nomte)
    goto 4
436  continue
    call te0336(nomopt, nomte)
    goto 4
437  continue
    call te0337(nomopt, nomte)
    goto 4
438  continue
    call te0338(nomopt, nomte)
    goto 4
439  continue
    call te0339(nomopt, nomte)
    goto 4
440  continue
    call te0340(nomopt, nomte)
    goto 4
441  continue
    call te0341(nomopt, nomte)
    goto 4
442  continue
    call te0342(nomopt, nomte)
    goto 4
443  continue
    call te0343(nomopt, nomte)
    goto 4
444  continue
    call te0344(nomopt, nomte)
    goto 4
445  continue
    call te0345(nomopt, nomte)
    goto 4
446  continue
    call te0346(nomopt, nomte)
    goto 4
447  continue
    call te0347(nomopt, nomte)
    goto 4
448  continue
    call te0348(nomopt, nomte)
    goto 4
449  continue
    call te0349(nomopt, nomte)
    goto 4
450  continue
    call te0350(nomopt, nomte)
    goto 4
451  continue
    call te0351(nomopt, nomte)
    goto 4
452  continue
    call te0352(nomopt, nomte)
    goto 4
453  continue
    call te0353(nomopt, nomte)
    goto 4
454  continue
    call te0354(nomopt, nomte)
    goto 4
455  continue
    call te0355(nomopt, nomte)
    goto 4
456  continue
    call te0356(nomopt, nomte)
    goto 4
457  continue
    call te0357(nomopt, nomte)
    goto 4
458  continue
    call te0358(nomopt, nomte)
    goto 4
459  continue
    call te0359(nomopt, nomte)
    goto 4
460  continue
    call te0360(nomopt, nomte)
    goto 4
461  continue
    call te0361(nomopt, nomte)
    goto 4
462  continue
    call te0362(nomopt, nomte)
    goto 4
463  continue
    call te0363(nomopt, nomte)
    goto 4
464  continue
    call te0364(nomopt, nomte)
    goto 4
465  continue
    call te0365(nomopt, nomte)
    goto 4
466  continue
    call te0366(nomopt, nomte)
    goto 4
467  continue
    call te0367(nomopt, nomte)
    goto 4
468  continue
    call te0368(nomopt, nomte)
    goto 4
469  continue
    call te0369(nomopt, nomte)
    goto 4
470  continue
    call te0370(nomopt, nomte)
    goto 4
471  continue
    call te0371(nomopt, nomte)
    goto 4
472  continue
    call te0372(nomopt, nomte)
    goto 4
473  continue
    call te0373(nomopt, nomte)
    goto 4
474  continue
    call te0374(nomopt, nomte)
    goto 4
475  continue
    call te0375(nomopt, nomte)
    goto 4
476  continue
    call te0376(nomopt, nomte)
    goto 4
477  continue
    call te0377(nomopt, nomte)
    goto 4
478  continue
    call te0378(nomopt, nomte)
    goto 4
479  continue
    call te0379(nomopt, nomte)
    goto 4
480  continue
    call te0380(nomopt, nomte)
    goto 4
481  continue
    call te0381(nomopt, nomte)
    goto 4
482  continue
    call te0382(nomopt, nomte)
    goto 4
483  continue
    call te0383(nomopt, nomte)
    goto 4
484  continue
    call te0384(nomopt, nomte)
    goto 4
485  continue
    call te0385(nomopt, nomte)
    goto 4
486  continue
    call te0386(nomopt, nomte)
    goto 4
487  continue
    call te0387(nomopt, nomte)
    goto 4
488  continue
    call te0388(nomopt, nomte)
    goto 4
489  continue
    call te0389(nomopt, nomte)
    goto 4
490  continue
    call te0390(nomopt, nomte)
    goto 4
491  continue
    call te0391(nomopt, nomte)
    goto 4
492  continue
    call te0392(nomopt, nomte)
    goto 4
493  continue
    call te0393(nomopt, nomte)
    goto 4
494  continue
    call te0394(nomopt, nomte)
    goto 4
495  continue
    call te0395(nomopt, nomte)
    goto 4
496  continue
    call te0396(nomopt, nomte)
    goto 4
497  continue
    call te0397(nomopt, nomte)
    goto 4
498  continue
    call te0398(nomopt, nomte)
    goto 4
499  continue
    call te0399(nomopt, nomte)
    goto 4
500  continue
    call te0400(nomopt, nomte)
    goto 4
    4 end do
    goto 9999
!
!
!     -- TE0401 --> TE0500
!     --------------------
9995  continue
    do 5, iel=1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) goto 5
    endif
    capoiz=0
    goto (501,502,503,504,505,506,507,508,509,510,511,512,513,514,&
        515,516,517,518,519,520,521,522,523,524,525,526,527,528,&
        529,530,531,532,533,534,535,536,537,538,539,540,541,542,&
        543,544,545,546,547,548,549,550,551,552,553,554,555,556,&
        557,558,559,560,561,562,563,564,565,566,567,568,569,570,&
        571,572,573,574,575,576,577,578,579,580,581,582,583,584,&
        585,586,587,588,589,590,591,592,593,594,595,596,597,598,&
        599,600) numc3
501  continue
    call te0401(nomopt, nomte)
    goto 5
502  continue
    call te0402(nomopt, nomte)
    goto 5
503  continue
    call te0403(nomopt, nomte)
    goto 5
504  continue
    call te0404(nomopt, nomte)
    goto 5
505  continue
    call te0405(nomopt, nomte)
    goto 5
506  continue
    call te0406(nomopt, nomte)
    goto 5
507  continue
    call te0407(nomopt, nomte)
    goto 5
508  continue
    call te0408(nomopt, nomte)
    goto 5
509  continue
    call te0409(nomopt, nomte)
    goto 5
510  continue
    call te0410(nomopt, nomte)
    goto 5
511  continue
    call te0411(nomopt, nomte)
    goto 5
512  continue
    call te0412(nomopt, nomte)
    goto 5
513  continue
    call te0413(nomopt, nomte)
    goto 5
514  continue
    call te0414(nomopt, nomte)
    goto 5
515  continue
    call te0415(nomopt, nomte)
    goto 5
516  continue
    call te0416(nomopt, nomte)
    goto 5
517  continue
    call te0417(nomopt, nomte)
    goto 5
518  continue
    call te0418(nomopt, nomte)
    goto 5
519  continue
    call te0419(nomopt, nomte)
    goto 5
520  continue
    call te0420(nomopt, nomte)
    goto 5
521  continue
    call te0421(nomopt, nomte)
    goto 5
522  continue
    call te0422(nomopt, nomte)
    goto 5
523  continue
    call te0423(nomopt, nomte)
    goto 5
524  continue
    call te0424(nomopt, nomte)
    goto 5
525  continue
    call te0425(nomopt, nomte)
    goto 5
526  continue
    call te0426(nomopt, nomte)
    goto 5
527  continue
    call te0427(nomopt, nomte)
    goto 5
528  continue
    call te0428(nomopt, nomte)
    goto 5
529  continue
    call te0429(nomopt, nomte)
    goto 5
530  continue
    call te0430(nomopt, nomte)
    goto 5
531  continue
    call te0431(nomopt, nomte)
    goto 5
532  continue
    call te0432(nomopt, nomte)
    goto 5
533  continue
    call te0433(nomopt, nomte)
    goto 5
534  continue
    call te0434(nomopt, nomte)
    goto 5
535  continue
    call te0435(nomopt, nomte)
    goto 5
536  continue
    call te0436(nomopt, nomte)
    goto 5
537  continue
    call te0437(nomopt, nomte)
    goto 5
538  continue
    call te0438(nomopt, nomte)
    goto 5
539  continue
    call te0439(nomopt, nomte)
    goto 5
540  continue
    call te0440(nomopt, nomte)
    goto 5
541  continue
    call te0441(nomopt, nomte)
    goto 5
542  continue
    call te0442(nomopt, nomte)
    goto 5
543  continue
    call te0443(nomopt, nomte)
    goto 5
544  continue
    call te0444(nomopt, nomte)
    goto 5
545  continue
    call te0445(nomopt, nomte)
    goto 5
546  continue
    call te0446(nomopt, nomte)
    goto 5
547  continue
    call te0447(nomopt, nomte)
    goto 5
548  continue
    call te0448(nomopt, nomte)
    goto 5
549  continue
    call te0449(nomopt, nomte)
    goto 5
550  continue
    call te0450(nomopt, nomte)
    goto 5
551  continue
    call te0451(nomopt, nomte)
    goto 5
552  continue
    call te0452(nomopt, nomte)
    goto 5
553  continue
    call te0453(nomopt, nomte)
    goto 5
554  continue
    call te0454(nomopt, nomte)
    goto 5
555  continue
    call te0455(nomopt, nomte)
    goto 5
556  continue
    call te0456(nomopt, nomte)
    goto 5
557  continue
    call te0457(nomopt, nomte)
    goto 5
558  continue
    call te0458(nomopt, nomte)
    goto 5
559  continue
    call te0459(nomopt, nomte)
    goto 5
560  continue
    call te0460(nomopt, nomte)
    goto 5
561  continue
    call te0461(nomopt, nomte)
    goto 5
562  continue
    call te0462(nomopt, nomte)
    goto 5
563  continue
    call te0463(nomopt, nomte)
    goto 5
564  continue
    call te0464(nomopt, nomte)
    goto 5
565  continue
    call te0465(nomopt, nomte)
    goto 5
566  continue
    call te0466(nomopt, nomte)
    goto 5
567  continue
    call te0467(nomopt, nomte)
    goto 5
568  continue
    call te0468(nomopt, nomte)
    goto 5
569  continue
    call te0469(nomopt, nomte)
    goto 5
570  continue
    call te0470(nomopt, nomte)
    goto 5
571  continue
    call te0471(nomopt, nomte)
    goto 5
572  continue
    call te0472(nomopt, nomte)
    goto 5
573  continue
    call te0473(nomopt, nomte)
    goto 5
574  continue
    call te0474(nomopt, nomte)
    goto 5
575  continue
    call te0475(nomopt, nomte)
    goto 5
576  continue
    call te0476(nomopt, nomte)
    goto 5
577  continue
    call te0477(nomopt, nomte)
    goto 5
578  continue
    call te0478(nomopt, nomte)
    goto 5
579  continue
    call te0479(nomopt, nomte)
    goto 5
580  continue
    call te0480(nomopt, nomte)
    goto 5
581  continue
    call te0481(nomopt, nomte)
    goto 5
582  continue
    call te0482(nomopt, nomte)
    goto 5
583  continue
    call te0483(nomopt, nomte)
    goto 5
584  continue
    call te0484(nomopt, nomte)
    goto 5
585  continue
    call te0485(nomopt, nomte)
    goto 5
586  continue
    call te0486(nomopt, nomte)
    goto 5
587  continue
    call te0487(nomopt, nomte)
    goto 5
588  continue
    call te0488(nomopt, nomte)
    goto 5
589  continue
    call te0489(nomopt, nomte)
    goto 5
590  continue
    call te0490(nomopt, nomte)
    goto 5
591  continue
    call te0491(nomopt, nomte)
    goto 5
592  continue
    call te0492(nomopt, nomte)
    goto 5
593  continue
    call te0493(nomopt, nomte)
    goto 5
594  continue
    call te0494(nomopt, nomte)
    goto 5
595  continue
    call te0495(nomopt, nomte)
    goto 5
596  continue
    call te0496(nomopt, nomte)
    goto 5
597  continue
    call te0497(nomopt, nomte)
    goto 5
598  continue
    call te0498(nomopt, nomte)
    goto 5
599  continue
    call te0499(nomopt, nomte)
    goto 5
600  continue
    call te0500(nomopt, nomte)
    goto 5
    5 end do
    goto 9999
!
!
!     -- TE0501 --> TE0600
!     --------------------
9996  continue
    do 6, iel=1,nbelgr
    if (lparal) then
        if (.not.zl(jparal-1+iel)) goto 6
    endif
    capoiz=0
    goto (601,602,603,604,605,606,607,608,609,610,611,612,613,614,&
        615,616,617,618,619,620,621,622,623,624,625,626,627,628,&
        629,630,631,632,633,634,635,636,637,638,639,640,641,642,&
        643,644,645,646,647,648,649,650,651,652,653,654,655,656,&
        657,658,659,660,661,662,663,664,665,666,667,668,669,670,&
        671,672,673,674,675,676,677,678,679,680,681,682,683,684,&
        685,686,687,688,689,690,691,692,693,694,695,696,697,698,&
        699,700) numc3
601  continue
    call te0501(nomopt, nomte)
    goto 6
602  continue
    call te0502(nomopt, nomte)
    goto 6
603  continue
    call te0503(nomopt, nomte)
    goto 6
604  continue
    call te0504(nomopt, nomte)
    goto 6
605  continue
    call te0505(nomopt, nomte)
    goto 6
606  continue
    call te0506(nomopt, nomte)
    goto 6
607  continue
    call te0507(nomopt, nomte)
    goto 6
608  continue
    call te0508(nomopt, nomte)
    goto 6
609  continue
    call te0509(nomopt, nomte)
    goto 6
610  continue
    call te0510(nomopt, nomte)
    goto 6
611  continue
    call te0511(nomopt, nomte)
    goto 6
612  continue
    call te0512(nomopt, nomte)
    goto 6
613  continue
    call te0513(nomopt, nomte)
    goto 6
614  continue
    call te0514(nomopt, nomte)
    goto 6
615  continue
    call te0515(nomopt, nomte)
    goto 6
616  continue
    call te0516(nomopt, nomte)
    goto 6
617  continue
    call te0517(nomopt, nomte)
    goto 6
618  continue
    call te0518(nomopt, nomte)
    goto 6
619  continue
    call te0519(nomopt, nomte)
    goto 6
620  continue
    call te0520(nomopt, nomte)
    goto 6
621  continue
    call te0521(nomopt, nomte)
    goto 6
622  continue
    call te0522(nomopt, nomte)
    goto 6
623  continue
    call te0523(nomopt, nomte)
    goto 6
624  continue
    call te0524(nomopt, nomte)
    goto 6
625  continue
    call te0525(nomopt, nomte)
    goto 6
626  continue
    call te0526(nomopt, nomte)
    goto 6
627  continue
    call te0527(nomopt, nomte)
    goto 6
628  continue
    call te0528(nomopt, nomte)
    goto 6
629  continue
    call te0529(nomopt, nomte)
    goto 6
630  continue
    call te0530(nomopt, nomte)
    goto 6
631  continue
    call te0531(nomopt, nomte)
    goto 6
632  continue
    call te0532(nomopt, nomte)
    goto 6
633  continue
    call te0533(nomopt, nomte)
    goto 6
634  continue
    call te0534(nomopt, nomte)
    goto 6
635  continue
    call te0535(nomopt, nomte)
    goto 6
636  continue
    call te0536(nomopt, nomte)
    goto 6
637  continue
    call te0537(nomopt, nomte)
    goto 6
638  continue
    call te0538(nomopt, nomte)
    goto 6
639  continue
    call te0539(nomopt, nomte)
    goto 6
640  continue
    call te0540(nomopt, nomte)
    goto 6
641  continue
    call te0541(nomopt, nomte)
    goto 6
642  continue
    call te0542(nomopt, nomte)
    goto 6
643  continue
    call te0543(nomopt, nomte)
    goto 6
644  continue
    call te0544(nomopt, nomte)
    goto 6
645  continue
    call te0545(nomopt, nomte)
    goto 6
646  continue
    call te0546(nomopt, nomte)
    goto 6
647  continue
    call te0547(nomopt, nomte)
    goto 6
648  continue
    call te0548(nomopt, nomte)
    goto 6
649  continue
    call te0549(nomopt, nomte)
    goto 6
650  continue
    call te0550(nomopt, nomte)
    goto 6
651  continue
    call te0551(nomopt, nomte)
    goto 6
652  continue
    call te0552(nomopt, nomte)
    goto 6
653  continue
    call te0553(nomopt, nomte)
    goto 6
654  continue
    call te0554(nomopt, nomte)
    goto 6
655  continue
    call te0555(nomopt, nomte)
    goto 6
656  continue
    call te0556(nomopt, nomte)
    goto 6
657  continue
    call te0557(nomopt, nomte)
    goto 6
658  continue
    call te0558(nomopt, nomte)
    goto 6
659  continue
    call te0559(nomopt, nomte)
    goto 6
660  continue
    call te0560(nomopt, nomte)
    goto 6
661  continue
    call te0561(nomopt, nomte)
    goto 6
662  continue
    call te0562(nomopt, nomte)
    goto 6
663  continue
    call te0563(nomopt, nomte)
    goto 6
664  continue
    call te0564(nomopt, nomte)
    goto 6
665  continue
    call te0565(nomopt, nomte)
    goto 6
666  continue
    call te0566(nomopt, nomte)
    goto 6
667  continue
    call te0567(nomopt, nomte)
    goto 6
668  continue
    call te0568(nomopt, nomte)
    goto 6
669  continue
    call te0569(nomopt, nomte)
    goto 6
670  continue
    call te0570(nomopt, nomte)
    goto 6
671  continue
    call te0571(nomopt, nomte)
    goto 6
672  continue
    call te0572(nomopt, nomte)
    goto 6
673  continue
    call te0573(nomopt, nomte)
    goto 6
674  continue
    call te0574(nomopt, nomte)
    goto 6
675  continue
    call te0575(nomopt, nomte)
    goto 6
676  continue
    call te0576(nomopt, nomte)
    goto 6
677  continue
    call te0577(nomopt, nomte)
    goto 6
678  continue
    call te0578(nomopt, nomte)
    goto 6
679  continue
    call te0579(nomopt, nomte)
    goto 6
680  continue
    call te0580(nomopt, nomte)
    goto 6
681  continue
    call te0581(nomopt, nomte)
    goto 6
682  continue
    call te0582(nomopt, nomte)
    goto 6
683  continue
    call te0583(nomopt, nomte)
    goto 6
684  continue
    call te0584(nomopt, nomte)
    goto 6
685  continue
    call te0585(nomopt, nomte)
    goto 6
686  continue
    call te0586(nomopt, nomte)
    goto 6
687  continue
    call te0587(nomopt, nomte)
    goto 6
688  continue
    call te0588(nomopt, nomte)
    goto 6
689  continue
    call te0589(nomopt, nomte)
    goto 6
690  continue
    call te0590(nomopt, nomte)
    goto 6
691  continue
    call te0591(nomopt, nomte)
    goto 6
692  continue
    call te0592(nomopt, nomte)
    goto 6
693  continue
    call te0593(nomopt, nomte)
    goto 6
694  continue
    call te0594(nomopt, nomte)
    goto 6
695  continue
    call te0595(nomopt, nomte)
    goto 6
696  continue
    call te0596(nomopt, nomte)
    goto 6
697  continue
    call te0597(nomopt, nomte)
    goto 6
698  continue
    call te0598(nomopt, nomte)
    goto 6
699  continue
    call te0599(nomopt, nomte)
    goto 6
700  continue
    call te0600(nomopt, nomte)
    goto 6
    6 end do
    goto 9999
!
!
9998  continue
    call codent(numc, 'D', k8bid)
    call u2mesk('F', 'CALCULEL4_91', 1, k8bid)
9999  continue
    call uttcpu('CPU.CALC.3', 'FIN', ' ')
end subroutine
