subroutine te0000(numc, opt, te)
! aslint: disable=W1501
use calcul_module, only : ca_capoiz_, ca_ianoop_, ca_ianote_, ca_iel_, ca_nbelgr_
implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/te0001.h"
#include "asterfort/te0002.h"
#include "asterfort/te0003.h"
#include "asterfort/te0004.h"
#include "asterfort/te0005.h"
#include "asterfort/te0006.h"
#include "asterfort/te0007.h"
#include "asterfort/te0008.h"
#include "asterfort/te0009.h"
#include "asterfort/te0010.h"
#include "asterfort/te0011.h"
#include "asterfort/te0012.h"
#include "asterfort/te0013.h"
#include "asterfort/te0014.h"
#include "asterfort/te0015.h"
#include "asterfort/te0016.h"
#include "asterfort/te0017.h"
#include "asterfort/te0018.h"
#include "asterfort/te0019.h"
#include "asterfort/te0020.h"
#include "asterfort/te0021.h"
#include "asterfort/te0022.h"
#include "asterfort/te0023.h"
#include "asterfort/te0024.h"
#include "asterfort/te0025.h"
#include "asterfort/te0026.h"
#include "asterfort/te0027.h"
#include "asterfort/te0028.h"
#include "asterfort/te0029.h"
#include "asterfort/te0030.h"
#include "asterfort/te0031.h"
#include "asterfort/te0032.h"
#include "asterfort/te0033.h"
#include "asterfort/te0034.h"
#include "asterfort/te0035.h"
#include "asterfort/te0036.h"
#include "asterfort/te0037.h"
#include "asterfort/te0038.h"
#include "asterfort/te0039.h"
#include "asterfort/te0040.h"
#include "asterfort/te0041.h"
#include "asterfort/te0042.h"
#include "asterfort/te0043.h"
#include "asterfort/te0044.h"
#include "asterfort/te0045.h"
#include "asterfort/te0046.h"
#include "asterfort/te0047.h"
#include "asterfort/te0048.h"
#include "asterfort/te0049.h"
#include "asterfort/te0050.h"
#include "asterfort/te0051.h"
#include "asterfort/te0052.h"
#include "asterfort/te0053.h"
#include "asterfort/te0054.h"
#include "asterfort/te0055.h"
#include "asterfort/te0056.h"
#include "asterfort/te0057.h"
#include "asterfort/te0058.h"
#include "asterfort/te0059.h"
#include "asterfort/te0060.h"
#include "asterfort/te0061.h"
#include "asterfort/te0062.h"
#include "asterfort/te0063.h"
#include "asterfort/te0064.h"
#include "asterfort/te0065.h"
#include "asterfort/te0066.h"
#include "asterfort/te0067.h"
#include "asterfort/te0068.h"
#include "asterfort/te0069.h"
#include "asterfort/te0070.h"
#include "asterfort/te0071.h"
#include "asterfort/te0072.h"
#include "asterfort/te0073.h"
#include "asterfort/te0074.h"
#include "asterfort/te0075.h"
#include "asterfort/te0076.h"
#include "asterfort/te0077.h"
#include "asterfort/te0078.h"
#include "asterfort/te0079.h"
#include "asterfort/te0080.h"
#include "asterfort/te0081.h"
#include "asterfort/te0082.h"
#include "asterfort/te0083.h"
#include "asterfort/te0084.h"
#include "asterfort/te0085.h"
#include "asterfort/te0086.h"
#include "asterfort/te0087.h"
#include "asterfort/te0088.h"
#include "asterfort/te0089.h"
#include "asterfort/te0090.h"
#include "asterfort/te0091.h"
#include "asterfort/te0092.h"
#include "asterfort/te0093.h"
#include "asterfort/te0094.h"
#include "asterfort/te0095.h"
#include "asterfort/te0096.h"
#include "asterfort/te0097.h"
#include "asterfort/te0098.h"
#include "asterfort/te0099.h"
#include "asterfort/te0100.h"
#include "asterfort/te0101.h"
#include "asterfort/te0102.h"
#include "asterfort/te0103.h"
#include "asterfort/te0104.h"
#include "asterfort/te0105.h"
#include "asterfort/te0106.h"
#include "asterfort/te0107.h"
#include "asterfort/te0108.h"
#include "asterfort/te0109.h"
#include "asterfort/te0110.h"
#include "asterfort/te0111.h"
#include "asterfort/te0112.h"
#include "asterfort/te0113.h"
#include "asterfort/te0114.h"
#include "asterfort/te0115.h"
#include "asterfort/te0116.h"
#include "asterfort/te0117.h"
#include "asterfort/te0118.h"
#include "asterfort/te0119.h"
#include "asterfort/te0120.h"
#include "asterfort/te0121.h"
#include "asterfort/te0122.h"
#include "asterfort/te0123.h"
#include "asterfort/te0124.h"
#include "asterfort/te0125.h"
#include "asterfort/te0126.h"
#include "asterfort/te0127.h"
#include "asterfort/te0128.h"
#include "asterfort/te0129.h"
#include "asterfort/te0130.h"
#include "asterfort/te0131.h"
#include "asterfort/te0132.h"
#include "asterfort/te0133.h"
#include "asterfort/te0134.h"
#include "asterfort/te0135.h"
#include "asterfort/te0136.h"
#include "asterfort/te0137.h"
#include "asterfort/te0138.h"
#include "asterfort/te0139.h"
#include "asterfort/te0140.h"
#include "asterfort/te0141.h"
#include "asterfort/te0142.h"
#include "asterfort/te0143.h"
#include "asterfort/te0144.h"
#include "asterfort/te0145.h"
#include "asterfort/te0146.h"
#include "asterfort/te0147.h"
#include "asterfort/te0148.h"
#include "asterfort/te0149.h"
#include "asterfort/te0150.h"
#include "asterfort/te0151.h"
#include "asterfort/te0152.h"
#include "asterfort/te0153.h"
#include "asterfort/te0154.h"
#include "asterfort/te0155.h"
#include "asterfort/te0156.h"
#include "asterfort/te0157.h"
#include "asterfort/te0158.h"
#include "asterfort/te0159.h"
#include "asterfort/te0160.h"
#include "asterfort/te0161.h"
#include "asterfort/te0162.h"
#include "asterfort/te0163.h"
#include "asterfort/te0164.h"
#include "asterfort/te0165.h"
#include "asterfort/te0166.h"
#include "asterfort/te0167.h"
#include "asterfort/te0168.h"
#include "asterfort/te0169.h"
#include "asterfort/te0170.h"
#include "asterfort/te0171.h"
#include "asterfort/te0172.h"
#include "asterfort/te0173.h"
#include "asterfort/te0174.h"
#include "asterfort/te0175.h"
#include "asterfort/te0176.h"
#include "asterfort/te0177.h"
#include "asterfort/te0178.h"
#include "asterfort/te0179.h"
#include "asterfort/te0180.h"
#include "asterfort/te0181.h"
#include "asterfort/te0182.h"
#include "asterfort/te0183.h"
#include "asterfort/te0184.h"
#include "asterfort/te0185.h"
#include "asterfort/te0186.h"
#include "asterfort/te0187.h"
#include "asterfort/te0188.h"
#include "asterfort/te0189.h"
#include "asterfort/te0190.h"
#include "asterfort/te0191.h"
#include "asterfort/te0192.h"
#include "asterfort/te0193.h"
#include "asterfort/te0194.h"
#include "asterfort/te0195.h"
#include "asterfort/te0196.h"
#include "asterfort/te0197.h"
#include "asterfort/te0198.h"
#include "asterfort/te0199.h"
#include "asterfort/te0200.h"
#include "asterfort/te0201.h"
#include "asterfort/te0202.h"
#include "asterfort/te0203.h"
#include "asterfort/te0204.h"
#include "asterfort/te0205.h"
#include "asterfort/te0206.h"
#include "asterfort/te0207.h"
#include "asterfort/te0208.h"
#include "asterfort/te0209.h"
#include "asterfort/te0210.h"
#include "asterfort/te0211.h"
#include "asterfort/te0212.h"
#include "asterfort/te0213.h"
#include "asterfort/te0214.h"
#include "asterfort/te0215.h"
#include "asterfort/te0216.h"
#include "asterfort/te0217.h"
#include "asterfort/te0218.h"
#include "asterfort/te0219.h"
#include "asterfort/te0220.h"
#include "asterfort/te0221.h"
#include "asterfort/te0222.h"
#include "asterfort/te0223.h"
#include "asterfort/te0224.h"
#include "asterfort/te0225.h"
#include "asterfort/te0226.h"
#include "asterfort/te0227.h"
#include "asterfort/te0228.h"
#include "asterfort/te0229.h"
#include "asterfort/te0230.h"
#include "asterfort/te0231.h"
#include "asterfort/te0232.h"
#include "asterfort/te0233.h"
#include "asterfort/te0234.h"
#include "asterfort/te0235.h"
#include "asterfort/te0236.h"
#include "asterfort/te0237.h"
#include "asterfort/te0238.h"
#include "asterfort/te0239.h"
#include "asterfort/te0240.h"
#include "asterfort/te0241.h"
#include "asterfort/te0242.h"
#include "asterfort/te0243.h"
#include "asterfort/te0244.h"
#include "asterfort/te0245.h"
#include "asterfort/te0246.h"
#include "asterfort/te0247.h"
#include "asterfort/te0248.h"
#include "asterfort/te0249.h"
#include "asterfort/te0250.h"
#include "asterfort/te0251.h"
#include "asterfort/te0252.h"
#include "asterfort/te0253.h"
#include "asterfort/te0254.h"
#include "asterfort/te0255.h"
#include "asterfort/te0256.h"
#include "asterfort/te0257.h"
#include "asterfort/te0258.h"
#include "asterfort/te0259.h"
#include "asterfort/te0260.h"
#include "asterfort/te0261.h"
#include "asterfort/te0262.h"
#include "asterfort/te0263.h"
#include "asterfort/te0264.h"
#include "asterfort/te0265.h"
#include "asterfort/te0266.h"
#include "asterfort/te0267.h"
#include "asterfort/te0268.h"
#include "asterfort/te0269.h"
#include "asterfort/te0270.h"
#include "asterfort/te0271.h"
#include "asterfort/te0272.h"
#include "asterfort/te0273.h"
#include "asterfort/te0274.h"
#include "asterfort/te0275.h"
#include "asterfort/te0276.h"
#include "asterfort/te0277.h"
#include "asterfort/te0278.h"
#include "asterfort/te0279.h"
#include "asterfort/te0280.h"
#include "asterfort/te0281.h"
#include "asterfort/te0282.h"
#include "asterfort/te0283.h"
#include "asterfort/te0284.h"
#include "asterfort/te0285.h"
#include "asterfort/te0286.h"
#include "asterfort/te0287.h"
#include "asterfort/te0288.h"
#include "asterfort/te0289.h"
#include "asterfort/te0290.h"
#include "asterfort/te0291.h"
#include "asterfort/te0292.h"
#include "asterfort/te0293.h"
#include "asterfort/te0294.h"
#include "asterfort/te0295.h"
#include "asterfort/te0296.h"
#include "asterfort/te0297.h"
#include "asterfort/te0298.h"
#include "asterfort/te0299.h"
#include "asterfort/te0300.h"
#include "asterfort/te0301.h"
#include "asterfort/te0302.h"
#include "asterfort/te0303.h"
#include "asterfort/te0304.h"
#include "asterfort/te0305.h"
#include "asterfort/te0306.h"
#include "asterfort/te0307.h"
#include "asterfort/te0308.h"
#include "asterfort/te0309.h"
#include "asterfort/te0310.h"
#include "asterfort/te0311.h"
#include "asterfort/te0312.h"
#include "asterfort/te0313.h"
#include "asterfort/te0314.h"
#include "asterfort/te0315.h"
#include "asterfort/te0316.h"
#include "asterfort/te0317.h"
#include "asterfort/te0318.h"
#include "asterfort/te0319.h"
#include "asterfort/te0320.h"
#include "asterfort/te0321.h"
#include "asterfort/te0322.h"
#include "asterfort/te0323.h"
#include "asterfort/te0324.h"
#include "asterfort/te0325.h"
#include "asterfort/te0326.h"
#include "asterfort/te0327.h"
#include "asterfort/te0328.h"
#include "asterfort/te0329.h"
#include "asterfort/te0330.h"
#include "asterfort/te0331.h"
#include "asterfort/te0332.h"
#include "asterfort/te0333.h"
#include "asterfort/te0334.h"
#include "asterfort/te0335.h"
#include "asterfort/te0336.h"
#include "asterfort/te0337.h"
#include "asterfort/te0338.h"
#include "asterfort/te0339.h"
#include "asterfort/te0340.h"
#include "asterfort/te0341.h"
#include "asterfort/te0342.h"
#include "asterfort/te0343.h"
#include "asterfort/te0344.h"
#include "asterfort/te0345.h"
#include "asterfort/te0346.h"
#include "asterfort/te0347.h"
#include "asterfort/te0348.h"
#include "asterfort/te0349.h"
#include "asterfort/te0350.h"
#include "asterfort/te0351.h"
#include "asterfort/te0352.h"
#include "asterfort/te0353.h"
#include "asterfort/te0354.h"
#include "asterfort/te0355.h"
#include "asterfort/te0356.h"
#include "asterfort/te0357.h"
#include "asterfort/te0358.h"
#include "asterfort/te0359.h"
#include "asterfort/te0360.h"
#include "asterfort/te0361.h"
#include "asterfort/te0362.h"
#include "asterfort/te0363.h"
#include "asterfort/te0364.h"
#include "asterfort/te0365.h"
#include "asterfort/te0366.h"
#include "asterfort/te0367.h"
#include "asterfort/te0368.h"
#include "asterfort/te0369.h"
#include "asterfort/te0370.h"
#include "asterfort/te0371.h"
#include "asterfort/te0372.h"
#include "asterfort/te0373.h"
#include "asterfort/te0374.h"
#include "asterfort/te0375.h"
#include "asterfort/te0376.h"
#include "asterfort/te0377.h"
#include "asterfort/te0378.h"
#include "asterfort/te0379.h"
#include "asterfort/te0380.h"
#include "asterfort/te0381.h"
#include "asterfort/te0382.h"
#include "asterfort/te0383.h"
#include "asterfort/te0384.h"
#include "asterfort/te0385.h"
#include "asterfort/te0386.h"
#include "asterfort/te0387.h"
#include "asterfort/te0388.h"
#include "asterfort/te0389.h"
#include "asterfort/te0390.h"
#include "asterfort/te0391.h"
#include "asterfort/te0392.h"
#include "asterfort/te0393.h"
#include "asterfort/te0394.h"
#include "asterfort/te0395.h"
#include "asterfort/te0396.h"
#include "asterfort/te0397.h"
#include "asterfort/te0398.h"
#include "asterfort/te0399.h"
#include "asterfort/te0400.h"
#include "asterfort/te0401.h"
#include "asterfort/te0402.h"
#include "asterfort/te0403.h"
#include "asterfort/te0404.h"
#include "asterfort/te0405.h"
#include "asterfort/te0406.h"
#include "asterfort/te0407.h"
#include "asterfort/te0408.h"
#include "asterfort/te0409.h"
#include "asterfort/te0410.h"
#include "asterfort/te0411.h"
#include "asterfort/te0412.h"
#include "asterfort/te0413.h"
#include "asterfort/te0414.h"
#include "asterfort/te0415.h"
#include "asterfort/te0416.h"
#include "asterfort/te0417.h"
#include "asterfort/te0418.h"
#include "asterfort/te0419.h"
#include "asterfort/te0420.h"
#include "asterfort/te0421.h"
#include "asterfort/te0422.h"
#include "asterfort/te0423.h"
#include "asterfort/te0424.h"
#include "asterfort/te0425.h"
#include "asterfort/te0426.h"
#include "asterfort/te0427.h"
#include "asterfort/te0428.h"
#include "asterfort/te0429.h"
#include "asterfort/te0430.h"
#include "asterfort/te0431.h"
#include "asterfort/te0432.h"
#include "asterfort/te0433.h"
#include "asterfort/te0434.h"
#include "asterfort/te0435.h"
#include "asterfort/te0436.h"
#include "asterfort/te0437.h"
#include "asterfort/te0438.h"
#include "asterfort/te0439.h"
#include "asterfort/te0440.h"
#include "asterfort/te0441.h"
#include "asterfort/te0442.h"
#include "asterfort/te0443.h"
#include "asterfort/te0444.h"
#include "asterfort/te0445.h"
#include "asterfort/te0446.h"
#include "asterfort/te0447.h"
#include "asterfort/te0448.h"
#include "asterfort/te0449.h"
#include "asterfort/te0450.h"
#include "asterfort/te0451.h"
#include "asterfort/te0452.h"
#include "asterfort/te0453.h"
#include "asterfort/te0454.h"
#include "asterfort/te0455.h"
#include "asterfort/te0456.h"
#include "asterfort/te0457.h"
#include "asterfort/te0458.h"
#include "asterfort/te0459.h"
#include "asterfort/te0460.h"
#include "asterfort/te0461.h"
#include "asterfort/te0462.h"
#include "asterfort/te0463.h"
#include "asterfort/te0464.h"
#include "asterfort/te0465.h"
#include "asterfort/te0466.h"
#include "asterfort/te0467.h"
#include "asterfort/te0468.h"
#include "asterfort/te0469.h"
#include "asterfort/te0470.h"
#include "asterfort/te0471.h"
#include "asterfort/te0472.h"
#include "asterfort/te0473.h"
#include "asterfort/te0474.h"
#include "asterfort/te0475.h"
#include "asterfort/te0476.h"
#include "asterfort/te0477.h"
#include "asterfort/te0478.h"
#include "asterfort/te0479.h"
#include "asterfort/te0480.h"
#include "asterfort/te0481.h"
#include "asterfort/te0482.h"
#include "asterfort/te0483.h"
#include "asterfort/te0484.h"
#include "asterfort/te0485.h"
#include "asterfort/te0486.h"
#include "asterfort/te0487.h"
#include "asterfort/te0488.h"
#include "asterfort/te0489.h"
#include "asterfort/te0490.h"
#include "asterfort/te0491.h"
#include "asterfort/te0492.h"
#include "asterfort/te0493.h"
#include "asterfort/te0494.h"
#include "asterfort/te0495.h"
#include "asterfort/te0496.h"
#include "asterfort/te0497.h"
#include "asterfort/te0498.h"
#include "asterfort/te0499.h"
#include "asterfort/te0500.h"
#include "asterfort/te0501.h"
#include "asterfort/te0502.h"
#include "asterfort/te0503.h"
#include "asterfort/te0504.h"
#include "asterfort/te0505.h"
#include "asterfort/te0506.h"
#include "asterfort/te0507.h"
#include "asterfort/te0508.h"
#include "asterfort/te0509.h"
#include "asterfort/te0510.h"
#include "asterfort/te0511.h"
#include "asterfort/te0512.h"
#include "asterfort/te0513.h"
#include "asterfort/te0514.h"
#include "asterfort/te0515.h"
#include "asterfort/te0516.h"
#include "asterfort/te0517.h"
#include "asterfort/te0518.h"
#include "asterfort/te0519.h"
#include "asterfort/te0520.h"
#include "asterfort/te0521.h"
#include "asterfort/te0522.h"
#include "asterfort/te0523.h"
#include "asterfort/te0524.h"
#include "asterfort/te0525.h"
#include "asterfort/te0526.h"
#include "asterfort/te0527.h"
#include "asterfort/te0528.h"
#include "asterfort/te0529.h"
#include "asterfort/te0530.h"
#include "asterfort/te0531.h"
#include "asterfort/te0532.h"
#include "asterfort/te0533.h"
#include "asterfort/te0534.h"
#include "asterfort/te0535.h"
#include "asterfort/te0536.h"
#include "asterfort/te0537.h"
#include "asterfort/te0538.h"
#include "asterfort/te0539.h"
#include "asterfort/te0540.h"
#include "asterfort/te0541.h"
#include "asterfort/te0542.h"
#include "asterfort/te0543.h"
#include "asterfort/te0544.h"
#include "asterfort/te0545.h"
#include "asterfort/te0546.h"
#include "asterfort/te0547.h"
#include "asterfort/te0548.h"
#include "asterfort/te0549.h"
#include "asterfort/te0550.h"
#include "asterfort/te0551.h"
#include "asterfort/te0552.h"
#include "asterfort/te0553.h"
#include "asterfort/te0554.h"
#include "asterfort/te0555.h"
#include "asterfort/te0556.h"
#include "asterfort/te0557.h"
#include "asterfort/te0558.h"
#include "asterfort/te0559.h"
#include "asterfort/te0560.h"
#include "asterfort/te0561.h"
#include "asterfort/te0562.h"
#include "asterfort/te0563.h"
#include "asterfort/te0564.h"
#include "asterfort/te0565.h"
#include "asterfort/te0566.h"
#include "asterfort/te0567.h"
#include "asterfort/te0568.h"
#include "asterfort/te0569.h"
#include "asterfort/te0570.h"
#include "asterfort/te0571.h"
#include "asterfort/te0572.h"
#include "asterfort/te0573.h"
#include "asterfort/te0574.h"
#include "asterfort/te0575.h"
#include "asterfort/te0576.h"
#include "asterfort/te0577.h"
#include "asterfort/te0578.h"
#include "asterfort/te0579.h"
#include "asterfort/te0580.h"
#include "asterfort/te0581.h"
#include "asterfort/te0582.h"
#include "asterfort/te0583.h"
#include "asterfort/te0584.h"
#include "asterfort/te0585.h"
#include "asterfort/te0586.h"
#include "asterfort/te0587.h"
#include "asterfort/te0588.h"
#include "asterfort/te0589.h"
#include "asterfort/te0590.h"
#include "asterfort/te0591.h"
#include "asterfort/te0592.h"
#include "asterfort/te0593.h"
#include "asterfort/te0594.h"
#include "asterfort/te0595.h"
#include "asterfort/te0596.h"
#include "asterfort/te0597.h"
#include "asterfort/te0598.h"
#include "asterfort/te0599.h"
#include "asterfort/te0600.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpu.h"
#include "asterfort/assert.h"
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
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: iret, jparal
    aster_logical :: lparal
    character(len=16) :: nomte, nomopt
    character(len=8) :: k8bid
! DEB-------------------------------------------------------------------
    call uttcpu('CPU.CALC.3', 'DEBUT', ' ')

!   -- avant d'augmenter la plage des numeros (au dela de 600),
!      il faut utiliser les numeros "vides" (les routines de 25 lignes)
    ASSERT(numc.gt.0)
    ASSERT(numc.le.600)
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
    nomte = zk16(ca_ianote_-1+te)
    nomopt = zk16(ca_ianoop_-1+opt)
!
    do ca_iel_ = 1, ca_nbelgr_
        if (lparal) then
            if (.not.zl(jparal-1+ca_iel_)) goto 999
        endif
        ca_capoiz_=0
        select case (numc)
        case (1)
            call te0001(nomopt, nomte)
        case (2)
            call te0002(nomopt, nomte)
        case (3)
            call te0003(nomopt, nomte)
        case (4)
            call te0004(nomopt, nomte)
        case (5)
            call te0005(nomopt, nomte)
        case (6)
            call te0006(nomopt, nomte)
        case (7)
            call te0007(nomopt, nomte)
        case (8)
            call te0008(nomopt, nomte)
        case (9)
            call te0009(nomopt, nomte)
        case (10)
            call te0010(nomopt, nomte)
        case (11)
            call te0011(nomopt, nomte)
        case (12)
            call te0012(nomopt, nomte)
        case (13)
            call te0013(nomopt, nomte)
        case (14)
            call te0014(nomopt, nomte)
        case (15)
            call te0015(nomopt, nomte)
        case (16)
            call te0016(nomopt, nomte)
        case (17)
            call te0017(nomopt, nomte)
        case (18)
            call te0018(nomopt, nomte)
        case (19)
            call te0019(nomopt, nomte)
        case (20)
            call te0020(nomopt, nomte)
        case (21)
            call te0021(nomopt, nomte)
        case (22)
            call te0022(nomopt, nomte)
        case (23)
            call te0023(nomopt, nomte)
        case (24)
            call te0024(nomopt, nomte)
        case (25)
            call te0025(nomopt, nomte)
        case (26)
            call te0026(nomopt, nomte)
        case (27)
            call te0027(nomopt, nomte)
        case (28)
            call te0028(nomopt, nomte)
        case (29)
            call te0029(nomopt, nomte)
        case (30)
            call te0030(nomopt, nomte)
        case (31)
            call te0031(nomopt, nomte)
        case (32)
            call te0032(nomopt, nomte)
        case (33)
            call te0033(nomopt, nomte)
        case (34)
            call te0034(nomopt, nomte)
        case (35)
            call te0035(nomopt, nomte)
        case (36)
            call te0036(nomopt, nomte)
        case (37)
            call te0037(nomopt, nomte)
        case (38)
            call te0038(nomopt, nomte)
        case (39)
            call te0039(nomopt, nomte)
        case (40)
            call te0040(nomopt, nomte)
        case (41)
            call te0041(nomopt, nomte)
        case (42)
            call te0042(nomopt, nomte)
        case (43)
            call te0043(nomopt, nomte)
        case (44)
            call te0044(nomopt, nomte)
        case (45)
            call te0045(nomopt, nomte)
        case (46)
            call te0046(nomopt, nomte)
        case (47)
            call te0047(nomopt, nomte)
        case (48)
            call te0048(nomopt, nomte)
        case (49)
            call te0049(nomopt, nomte)
        case (50)
            call te0050(nomopt, nomte)
        case (51)
            call te0051(nomopt, nomte)
        case (52)
            call te0052(nomopt, nomte)
        case (53)
            call te0053(nomopt, nomte)
        case (54)
            call te0054(nomopt, nomte)
        case (55)
            call te0055(nomopt, nomte)
        case (56)
            call te0056(nomopt, nomte)
        case (57)
            call te0057(nomopt, nomte)
        case (58)
            call te0058(nomopt, nomte)
        case (59)
            call te0059(nomopt, nomte)
        case (60)
            call te0060(nomopt, nomte)
        case (61)
            call te0061(nomopt, nomte)
        case (62)
            call te0062(nomopt, nomte)
        case (63)
            call te0063(nomopt, nomte)
        case (64)
            call te0064(nomopt, nomte)
        case (65)
            call te0065(nomopt, nomte)
        case (66)
            call te0066(nomopt, nomte)
        case (67)
            call te0067(nomopt, nomte)
        case (68)
            call te0068(nomopt, nomte)
        case (69)
            call te0069(nomopt, nomte)
        case (70)
            call te0070(nomopt, nomte)
        case (71)
            call te0071(nomopt, nomte)
        case (72)
            call te0072(nomopt, nomte)
        case (73)
            call te0073(nomopt, nomte)
        case (74)
            call te0074(nomopt, nomte)
        case (75)
            call te0075(nomopt, nomte)
        case (76)
            call te0076(nomopt, nomte)
        case (77)
            call te0077(nomopt, nomte)
        case (78)
            call te0078(nomopt, nomte)
        case (79)
            call te0079(nomopt, nomte)
        case (80)
            call te0080(nomopt, nomte)
        case (81)
            call te0081(nomopt, nomte)
        case (82)
            call te0082(nomopt, nomte)
        case (83)
            call te0083(nomopt, nomte)
        case (84)
            call te0084(nomopt, nomte)
        case (85)
            call te0085(nomopt, nomte)
        case (86)
            call te0086(nomopt, nomte)
        case (87)
            call te0087(nomopt, nomte)
        case (88)
            call te0088(nomopt, nomte)
        case (89)
            call te0089(nomopt, nomte)
        case (90)
            call te0090(nomopt, nomte)
        case (91)
            call te0091(nomopt, nomte)
        case (92)
            call te0092(nomopt, nomte)
        case (93)
            call te0093(nomopt, nomte)
        case (94)
            call te0094(nomopt, nomte)
        case (95)
            call te0095(nomopt, nomte)
        case (96)
            call te0096(nomopt, nomte)
        case (97)
            call te0097(nomopt, nomte)
        case (98)
            call te0098(nomopt, nomte)
        case (99)
            call te0099(nomopt, nomte)
        case (100)
            call te0100(nomopt, nomte)
        case (101)
            call te0101(nomopt, nomte)
        case (102)
            call te0102(nomopt, nomte)
        case (103)
            call te0103(nomopt, nomte)
        case (104)
            call te0104(nomopt, nomte)
        case (105)
            call te0105(nomopt, nomte)
        case (106)
            call te0106(nomopt, nomte)
        case (107)
            call te0107(nomopt, nomte)
        case (108)
            call te0108(nomopt, nomte)
        case (109)
            call te0109(nomopt, nomte)
        case (110)
            call te0110(nomopt, nomte)
        case (111)
            call te0111(nomopt, nomte)
        case (112)
            call te0112(nomopt, nomte)
        case (113)
            call te0113(nomopt, nomte)
        case (114)
            call te0114(nomopt, nomte)
        case (115)
            call te0115(nomopt, nomte)
        case (116)
            call te0116(nomopt, nomte)
        case (117)
            call te0117(nomopt, nomte)
        case (118)
            call te0118(nomopt, nomte)
        case (119)
            call te0119(nomopt, nomte)
        case (120)
            call te0120(nomopt, nomte)
        case (121)
            call te0121(nomopt, nomte)
        case (122)
            call te0122(nomopt, nomte)
        case (123)
            call te0123(nomopt, nomte)
        case (124)
            call te0124(nomopt, nomte)
        case (125)
            call te0125(nomopt, nomte)
        case (126)
            call te0126(nomopt, nomte)
        case (127)
            call te0127(nomopt, nomte)
        case (128)
            call te0128(nomopt, nomte)
        case (129)
            call te0129(nomopt, nomte)
        case (130)
            call te0130(nomopt, nomte)
        case (131)
            call te0131(nomopt, nomte)
        case (132)
            call te0132(nomopt, nomte)
        case (133)
            call te0133(nomopt, nomte)
        case (134)
            call te0134(nomopt, nomte)
        case (135)
            call te0135(nomopt, nomte)
        case (136)
            call te0136(nomopt, nomte)
        case (137)
            call te0137(nomopt, nomte)
        case (138)
            call te0138(nomopt, nomte)
        case (139)
            call te0139(nomopt, nomte)
        case (140)
            call te0140(nomopt, nomte)
        case (141)
            call te0141(nomopt, nomte)
        case (142)
            call te0142(nomopt, nomte)
        case (143)
            call te0143(nomopt, nomte)
        case (144)
            call te0144(nomopt, nomte)
        case (145)
            call te0145(nomopt, nomte)
        case (146)
            call te0146(nomopt, nomte)
        case (147)
            call te0147(nomopt, nomte)
        case (148)
            call te0148(nomopt, nomte)
        case (149)
            call te0149(nomopt, nomte)
        case (150)
            call te0150(nomopt, nomte)
        case (151)
            call te0151(nomopt, nomte)
        case (152)
            call te0152(nomopt, nomte)
        case (153)
            call te0153(nomopt, nomte)
        case (154)
            call te0154(nomopt, nomte)
        case (155)
            call te0155(nomopt, nomte)
        case (156)
            call te0156(nomopt, nomte)
        case (157)
            call te0157(nomopt, nomte)
        case (158)
            call te0158(nomopt, nomte)
        case (159)
            call te0159(nomopt, nomte)
        case (160)
            call te0160(nomopt, nomte)
        case (161)
            call te0161(nomopt, nomte)
        case (162)
            call te0162(nomopt, nomte)
        case (163)
            call te0163(nomopt, nomte)
        case (164)
            call te0164(nomopt, nomte)
        case (165)
            call te0165(nomopt, nomte)
        case (166)
            call te0166(nomopt, nomte)
        case (167)
            call te0167(nomopt, nomte)
        case (168)
            call te0168(nomopt, nomte)
        case (169)
            call te0169(nomopt, nomte)
        case (170)
            call te0170(nomopt, nomte)
        case (171)
            call te0171(nomopt, nomte)
        case (172)
            call te0172(nomopt, nomte)
        case (173)
            call te0173(nomopt, nomte)
        case (174)
            call te0174(nomopt, nomte)
        case (175)
            call te0175(nomopt, nomte)
        case (176)
            call te0176(nomopt, nomte)
        case (177)
            call te0177(nomopt, nomte)
        case (178)
            call te0178(nomopt, nomte)
        case (179)
            call te0179(nomopt, nomte)
        case (180)
            call te0180(nomopt, nomte)
        case (181)
            call te0181(nomopt, nomte)
        case (182)
            call te0182(nomopt, nomte)
        case (183)
            call te0183(nomopt, nomte)
        case (184)
            call te0184(nomopt, nomte)
        case (185)
            call te0185(nomopt, nomte)
        case (186)
            call te0186(nomopt, nomte)
        case (187)
            call te0187(nomopt, nomte)
        case (188)
            call te0188(nomopt, nomte)
        case (189)
            call te0189(nomopt, nomte)
        case (190)
            call te0190(nomopt, nomte)
        case (191)
            call te0191(nomopt, nomte)
        case (192)
            call te0192(nomopt, nomte)
        case (193)
            call te0193(nomopt, nomte)
        case (194)
            call te0194(nomopt, nomte)
        case (195)
            call te0195(nomopt, nomte)
        case (196)
            call te0196(nomopt, nomte)
        case (197)
            call te0197(nomopt, nomte)
        case (198)
            call te0198(nomopt, nomte)
        case (199)
            call te0199(nomopt, nomte)
        case (200)
            call te0200(nomopt, nomte)
        case (201)
            call te0201(nomopt, nomte)
        case (202)
            call te0202(nomopt, nomte)
        case (203)
            call te0203(nomopt, nomte)
        case (204)
            call te0204(nomopt, nomte)
        case (205)
            call te0205(nomopt, nomte)
        case (206)
            call te0206(nomopt, nomte)
        case (207)
            call te0207(nomopt, nomte)
        case (208)
            call te0208(nomopt, nomte)
        case (209)
            call te0209(nomopt, nomte)
        case (210)
            call te0210(nomopt, nomte)
        case (211)
            call te0211(nomopt, nomte)
        case (212)
            call te0212(nomopt, nomte)
        case (213)
            call te0213(nomopt, nomte)
        case (214)
            call te0214(nomopt, nomte)
        case (215)
            call te0215(nomopt, nomte)
        case (216)
            call te0216(nomopt, nomte)
        case (217)
            call te0217(nomopt, nomte)
        case (218)
            call te0218(nomopt, nomte)
        case (219)
            call te0219(nomopt, nomte)
        case (220)
            call te0220(nomopt, nomte)
        case (221)
            call te0221(nomopt, nomte)
        case (222)
            call te0222(nomopt, nomte)
        case (223)
            call te0223(nomopt, nomte)
        case (224)
            call te0224(nomopt, nomte)
        case (225)
            call te0225(nomopt, nomte)
        case (226)
            call te0226(nomopt, nomte)
        case (227)
            call te0227(nomopt, nomte)
        case (228)
            call te0228(nomopt, nomte)
        case (229)
            call te0229(nomopt, nomte)
        case (230)
            call te0230(nomopt, nomte)
        case (231)
            call te0231(nomopt, nomte)
        case (232)
            call te0232(nomopt, nomte)
        case (233)
            call te0233(nomopt, nomte)
        case (234)
            call te0234(nomopt, nomte)
        case (235)
            call te0235(nomopt, nomte)
        case (236)
            call te0236(nomopt, nomte)
        case (237)
            call te0237(nomopt, nomte)
        case (238)
            call te0238(nomopt, nomte)
        case (239)
            call te0239(nomopt, nomte)
        case (240)
            call te0240(nomopt, nomte)
        case (241)
            call te0241(nomopt, nomte)
        case (242)
            call te0242(nomopt, nomte)
        case (243)
            call te0243(nomopt, nomte)
        case (244)
            call te0244(nomopt, nomte)
        case (245)
            call te0245(nomopt, nomte)
        case (246)
            call te0246(nomopt, nomte)
        case (247)
            call te0247(nomopt, nomte)
        case (248)
            call te0248(nomopt, nomte)
        case (249)
            call te0249(nomopt, nomte)
        case (250)
            call te0250(nomopt, nomte)
        case (251)
            call te0251(nomopt, nomte)
        case (252)
            call te0252(nomopt, nomte)
        case (253)
            call te0253(nomopt, nomte)
        case (254)
            call te0254(nomopt, nomte)
        case (255)
            call te0255(nomopt, nomte)
        case (256)
            call te0256(nomopt, nomte)
        case (257)
            call te0257(nomopt, nomte)
        case (258)
            call te0258(nomopt, nomte)
        case (259)
            call te0259(nomopt, nomte)
        case (260)
            call te0260(nomopt, nomte)
        case (261)
            call te0261(nomopt, nomte)
        case (262)
            call te0262(nomopt, nomte)
        case (263)
            call te0263(nomopt, nomte)
        case (264)
            call te0264(nomopt, nomte)
        case (265)
            call te0265(nomopt, nomte)
        case (266)
            call te0266(nomopt, nomte)
        case (267)
            call te0267(nomopt, nomte)
        case (268)
            call te0268(nomopt, nomte)
        case (269)
            call te0269(nomopt, nomte)
        case (270)
            call te0270(nomopt, nomte)
        case (271)
            call te0271(nomopt, nomte)
        case (272)
            call te0272(nomopt, nomte)
        case (273)
            call te0273(nomopt, nomte)
        case (274)
            call te0274(nomopt, nomte)
        case (275)
            call te0275(nomopt, nomte)
        case (276)
            call te0276(nomopt, nomte)
        case (277)
            call te0277(nomopt, nomte)
        case (278)
            call te0278(nomopt, nomte)
        case (279)
            call te0279(nomopt, nomte)
        case (280)
            call te0280(nomopt, nomte)
        case (281)
            call te0281(nomopt, nomte)
        case (282)
            call te0282(nomopt, nomte)
        case (283)
            call te0283(nomopt, nomte)
        case (284)
            call te0284(nomopt, nomte)
        case (285)
            call te0285(nomopt, nomte)
        case (286)
            call te0286(nomopt, nomte)
        case (287)
            call te0287(nomopt, nomte)
        case (288)
            call te0288(nomopt, nomte)
        case (289)
            call te0289(nomopt, nomte)
        case (290)
            call te0290(nomopt, nomte)
        case (291)
            call te0291(nomopt, nomte)
        case (292)
            call te0292(nomopt, nomte)
        case (293)
            call te0293(nomopt, nomte)
        case (294)
            call te0294(nomopt, nomte)
        case (295)
            call te0295(nomopt, nomte)
        case (296)
            call te0296(nomopt, nomte)
        case (297)
            call te0297(nomopt, nomte)
        case (298)
            call te0298(nomopt, nomte)
        case (299)
            call te0299(nomopt, nomte)
        case (300)
            call te0300(nomopt, nomte)
        case (301)
            call te0301(nomopt, nomte)
        case (302)
            call te0302(nomopt, nomte)
        case (303)
            call te0303(nomopt, nomte)
        case (304)
            call te0304(nomopt, nomte)
        case (305)
            call te0305(nomopt, nomte)
        case (306)
            call te0306(nomopt, nomte)
        case (307)
            call te0307(nomopt, nomte)
        case (308)
            call te0308(nomopt, nomte)
        case (309)
            call te0309(nomopt, nomte)
        case (310)
            call te0310(nomopt, nomte)
        case (311)
            call te0311(nomopt, nomte)
        case (312)
            call te0312(nomopt, nomte)
        case (313)
            call te0313(nomopt, nomte)
        case (314)
            call te0314(nomopt, nomte)
        case (315)
            call te0315(nomopt, nomte)
        case (316)
            call te0316(nomopt, nomte)
        case (317)
            call te0317(nomopt, nomte)
        case (318)
            call te0318(nomopt, nomte)
        case (319)
            call te0319(nomopt, nomte)
        case (320)
            call te0320(nomopt, nomte)
        case (321)
            call te0321(nomopt, nomte)
        case (322)
            call te0322(nomopt, nomte)
        case (323)
            call te0323(nomopt, nomte)
        case (324)
            call te0324(nomopt, nomte)
        case (325)
            call te0325(nomopt, nomte)
        case (326)
            call te0326(nomopt, nomte)
        case (327)
            call te0327(nomopt, nomte)
        case (328)
            call te0328(nomopt, nomte)
        case (329)
            call te0329(nomopt, nomte)
        case (330)
            call te0330(nomopt, nomte)
        case (331)
            call te0331(nomopt, nomte)
        case (332)
            call te0332(nomopt, nomte)
        case (333)
            call te0333(nomopt, nomte)
        case (334)
            call te0334(nomopt, nomte)
        case (335)
            call te0335(nomopt, nomte)
        case (336)
            call te0336(nomopt, nomte)
        case (337)
            call te0337(nomopt, nomte)
        case (338)
            call te0338(nomopt, nomte)
        case (339)
            call te0339(nomopt, nomte)
        case (340)
            call te0340(nomopt, nomte)
        case (341)
            call te0341(nomopt, nomte)
        case (342)
            call te0342(nomopt, nomte)
        case (343)
            call te0343(nomopt, nomte)
        case (344)
            call te0344(nomopt, nomte)
        case (345)
            call te0345(nomopt, nomte)
        case (346)
            call te0346(nomopt, nomte)
        case (347)
            call te0347(nomopt, nomte)
        case (348)
            call te0348(nomopt, nomte)
        case (349)
            call te0349(nomopt, nomte)
        case (350)
            call te0350(nomopt, nomte)
        case (351)
            call te0351(nomopt, nomte)
        case (352)
            call te0352(nomopt, nomte)
        case (353)
            call te0353(nomopt, nomte)
        case (354)
            call te0354(nomopt, nomte)
        case (355)
            call te0355(nomopt, nomte)
        case (356)
            call te0356(nomopt, nomte)
        case (357)
            call te0357(nomopt, nomte)
        case (358)
            call te0358(nomopt, nomte)
        case (359)
            call te0359(nomopt, nomte)
        case (360)
            call te0360(nomopt, nomte)
        case (361)
            call te0361(nomopt, nomte)
        case (362)
            call te0362(nomopt, nomte)
        case (363)
            call te0363(nomopt, nomte)
        case (364)
            call te0364(nomopt, nomte)
        case (365)
            call te0365(nomopt, nomte)
        case (366)
            call te0366(nomopt, nomte)
        case (367)
            call te0367(nomopt, nomte)
        case (368)
            call te0368(nomopt, nomte)
        case (369)
            call te0369(nomopt, nomte)
        case (370)
            call te0370(nomopt, nomte)
        case (371)
            call te0371(nomopt, nomte)
        case (372)
            call te0372(nomopt, nomte)
        case (373)
            call te0373(nomopt, nomte)
        case (374)
            call te0374(nomopt, nomte)
        case (375)
            call te0375(nomopt, nomte)
        case (376)
            call te0376(nomopt, nomte)
        case (377)
            call te0377(nomopt, nomte)
        case (378)
            call te0378(nomopt, nomte)
        case (379)
            call te0379(nomopt, nomte)
        case (380)
            call te0380(nomopt, nomte)
        case (381)
            call te0381(nomopt, nomte)
        case (382)
            call te0382(nomopt, nomte)
        case (383)
            call te0383(nomopt, nomte)
        case (384)
            call te0384(nomopt, nomte)
        case (385)
            call te0385(nomopt, nomte)
        case (386)
            call te0386(nomopt, nomte)
        case (387)
            call te0387(nomopt, nomte)
        case (388)
            call te0388(nomopt, nomte)
        case (389)
            call te0389(nomopt, nomte)
        case (390)
            call te0390(nomopt, nomte)
        case (391)
            call te0391(nomopt, nomte)
        case (392)
            call te0392(nomopt, nomte)
        case (393)
            call te0393(nomopt, nomte)
        case (394)
            call te0394(nomopt, nomte)
        case (395)
            call te0395(nomopt, nomte)
        case (396)
            call te0396(nomopt, nomte)
        case (397)
            call te0397(nomopt, nomte)
        case (398)
            call te0398(nomopt, nomte)
        case (399)
            call te0399(nomopt, nomte)
        case (400)
            call te0400(nomopt, nomte)
        case (401)
            call te0401(nomopt, nomte)
        case (402)
            call te0402(nomopt, nomte)
        case (403)
            call te0403(nomopt, nomte)
        case (404)
            call te0404(nomopt, nomte)
        case (405)
            call te0405(nomopt, nomte)
        case (406)
            call te0406(nomopt, nomte)
        case (407)
            call te0407(nomopt, nomte)
        case (408)
            call te0408(nomopt, nomte)
        case (409)
            call te0409(nomopt, nomte)
        case (410)
            call te0410(nomopt, nomte)
        case (411)
            call te0411(nomopt, nomte)
        case (412)
            call te0412(nomopt, nomte)
        case (413)
            call te0413(nomopt, nomte)
        case (414)
            call te0414(nomopt, nomte)
        case (415)
            call te0415(nomopt, nomte)
        case (416)
            call te0416(nomopt, nomte)
        case (417)
            call te0417(nomopt, nomte)
        case (418)
            call te0418(nomopt, nomte)
        case (419)
            call te0419(nomopt, nomte)
        case (420)
            call te0420(nomopt, nomte)
        case (421)
            call te0421(nomopt, nomte)
        case (422)
            call te0422(nomopt, nomte)
        case (423)
            call te0423(nomopt, nomte)
        case (424)
            call te0424(nomopt, nomte)
        case (425)
            call te0425(nomopt, nomte)
        case (426)
            call te0426(nomopt, nomte)
        case (427)
            call te0427(nomopt, nomte)
        case (428)
            call te0428(nomopt, nomte)
        case (429)
            call te0429(nomopt, nomte)
        case (430)
            call te0430(nomopt, nomte)
        case (431)
            call te0431(nomopt, nomte)
        case (432)
            call te0432(nomopt, nomte)
        case (433)
            call te0433(nomopt, nomte)
        case (434)
            call te0434(nomopt, nomte)
        case (435)
            call te0435(nomopt, nomte)
        case (436)
            call te0436(nomopt, nomte)
        case (437)
            call te0437(nomopt, nomte)
        case (438)
            call te0438(nomopt, nomte)
        case (439)
            call te0439(nomopt, nomte)
        case (440)
            call te0440(nomopt, nomte)
        case (441)
            call te0441(nomopt, nomte)
        case (442)
            call te0442(nomopt, nomte)
        case (443)
            call te0443(nomopt, nomte)
        case (444)
            call te0444(nomopt, nomte)
        case (445)
            call te0445(nomopt, nomte)
        case (446)
            call te0446(nomopt, nomte)
        case (447)
            call te0447(nomopt, nomte)
        case (448)
            call te0448(nomopt, nomte)
        case (449)
            call te0449(nomopt, nomte)
        case (450)
            call te0450(nomopt, nomte)
        case (451)
            call te0451(nomopt, nomte)
        case (452)
            call te0452(nomopt, nomte)
        case (453)
            call te0453(nomopt, nomte)
        case (454)
            call te0454(nomopt, nomte)
        case (455)
            call te0455(nomopt, nomte)
        case (456)
            call te0456(nomopt, nomte)
        case (457)
            call te0457(nomopt, nomte)
        case (458)
            call te0458(nomopt, nomte)
        case (459)
            call te0459(nomopt, nomte)
        case (460)
            call te0460(nomopt, nomte)
        case (461)
            call te0461(nomopt, nomte)
        case (462)
            call te0462(nomopt, nomte)
        case (463)
            call te0463(nomopt, nomte)
        case (464)
            call te0464(nomopt, nomte)
        case (465)
            call te0465(nomopt, nomte)
        case (466)
            call te0466(nomopt, nomte)
        case (467)
            call te0467(nomopt, nomte)
        case (468)
            call te0468(nomopt, nomte)
        case (469)
            call te0469(nomopt, nomte)
        case (470)
            call te0470(nomopt, nomte)
        case (471)
            call te0471(nomopt, nomte)
        case (472)
            call te0472(nomopt, nomte)
        case (473)
            call te0473(nomopt, nomte)
        case (474)
            call te0474(nomopt, nomte)
        case (475)
            call te0475(nomopt, nomte)
        case (476)
            call te0476(nomopt, nomte)
        case (477)
            call te0477(nomopt, nomte)
        case (478)
            call te0478(nomopt, nomte)
        case (479)
            call te0479(nomopt, nomte)
        case (480)
            call te0480(nomopt, nomte)
        case (481)
            call te0481(nomopt, nomte)
        case (482)
            call te0482(nomopt, nomte)
        case (483)
            call te0483(nomopt, nomte)
        case (484)
            call te0484(nomopt, nomte)
        case (485)
            call te0485(nomopt, nomte)
        case (486)
            call te0486(nomopt, nomte)
        case (487)
            call te0487(nomopt, nomte)
        case (488)
            call te0488(nomopt, nomte)
        case (489)
            call te0489(nomopt, nomte)
        case (490)
            call te0490(nomopt, nomte)
        case (491)
            call te0491(nomopt, nomte)
        case (492)
            call te0492(nomopt, nomte)
        case (493)
            call te0493(nomopt, nomte)
        case (494)
            call te0494(nomopt, nomte)
        case (495)
            call te0495(nomopt, nomte)
        case (496)
            call te0496(nomopt, nomte)
        case (497)
            call te0497(nomopt, nomte)
        case (498)
            call te0498(nomopt, nomte)
        case (499)
            call te0499(nomopt, nomte)
        case (500)
            call te0500(nomopt, nomte)
        case (501)
            call te0501(nomopt, nomte)
        case (502)
            call te0502(nomopt, nomte)
        case (503)
            call te0503(nomopt, nomte)
        case (504)
            call te0504(nomopt, nomte)
        case (505)
            call te0505(nomopt, nomte)
        case (506)
            call te0506(nomopt, nomte)
        case (507)
            call te0507(nomopt, nomte)
        case (508)
            call te0508(nomopt, nomte)
        case (509)
            call te0509(nomopt, nomte)
        case (510)
            call te0510(nomopt, nomte)
        case (511)
            call te0511(nomopt, nomte)
        case (512)
            call te0512(nomopt, nomte)
        case (513)
            call te0513(nomopt, nomte)
        case (514)
            call te0514(nomopt, nomte)
        case (515)
            call te0515(nomopt, nomte)
        case (516)
            call te0516(nomopt, nomte)
        case (517)
            call te0517(nomopt, nomte)
        case (518)
            call te0518(nomopt, nomte)
        case (519)
            call te0519(nomopt, nomte)
        case (520)
            call te0520(nomopt, nomte)
        case (521)
            call te0521(nomopt, nomte)
        case (522)
            call te0522(nomopt, nomte)
        case (523)
            call te0523(nomopt, nomte)
        case (524)
            call te0524(nomopt, nomte)
        case (525)
            call te0525(nomopt, nomte)
        case (526)
            call te0526(nomopt, nomte)
        case (527)
            call te0527(nomopt, nomte)
        case (528)
            call te0528(nomopt, nomte)
        case (529)
            call te0529(nomopt, nomte)
        case (530)
            call te0530(nomopt, nomte)
        case (531)
            call te0531(nomopt, nomte)
        case (532)
            call te0532(nomopt, nomte)
        case (533)
            call te0533(nomopt, nomte)
        case (534)
            call te0534(nomopt, nomte)
        case (535)
            call te0535(nomopt, nomte)
        case (536)
            call te0536(nomopt, nomte)
        case (537)
            call te0537(nomopt, nomte)
        case (538)
            call te0538(nomopt, nomte)
        case (539)
            call te0539(nomopt, nomte)
        case (540)
            call te0540(nomopt, nomte)
        case (541)
            call te0541(nomopt, nomte)
        case (542)
            call te0542(nomopt, nomte)
        case (543)
            call te0543(nomopt, nomte)
        case (544)
            call te0544(nomopt, nomte)
        case (545)
            call te0545(nomopt, nomte)
        case (546)
            call te0546(nomopt, nomte)
        case (547)
            call te0547(nomopt, nomte)
        case (548)
            call te0548(nomopt, nomte)
        case (549)
            call te0549(nomopt, nomte)
        case (550)
            call te0550(nomopt, nomte)
        case (551)
            call te0551(nomopt, nomte)
        case (552)
            call te0552(nomopt, nomte)
        case (553)
            call te0553(nomopt, nomte)
        case (554)
            call te0554(nomopt, nomte)
        case (555)
            call te0555(nomopt, nomte)
        case (556)
            call te0556(nomopt, nomte)
        case (557)
            call te0557(nomopt, nomte)
        case (558)
            call te0558(nomopt, nomte)
        case (559)
            call te0559(nomopt, nomte)
        case (560)
            call te0560(nomopt, nomte)
        case (561)
            call te0561(nomopt, nomte)
        case (562)
            call te0562(nomopt, nomte)
        case (563)
            call te0563(nomopt, nomte)
        case (564)
            call te0564(nomopt, nomte)
        case (565)
            call te0565(nomopt, nomte)
        case (566)
            call te0566(nomopt, nomte)
        case (567)
            call te0567(nomopt, nomte)
        case (568)
            call te0568(nomopt, nomte)
        case (569)
            call te0569(nomopt, nomte)
        case (570)
            call te0570(nomopt, nomte)
        case (571)
            call te0571(nomopt, nomte)
        case (572)
            call te0572(nomopt, nomte)
        case (573)
            call te0573(nomopt, nomte)
        case (574)
            call te0574(nomopt, nomte)
        case (575)
            call te0575(nomopt, nomte)
        case (576)
            call te0576(nomopt, nomte)
        case (577)
            call te0577(nomopt, nomte)
        case (578)
            call te0578(nomopt, nomte)
        case (579)
            call te0579(nomopt, nomte)
        case (580)
            call te0580(nomopt, nomte)
        case (581)
            call te0581(nomopt, nomte)
        case (582)
            call te0582(nomopt, nomte)
        case (583)
            call te0583(nomopt, nomte)
        case (584)
            call te0584(nomopt, nomte)
        case (585)
            call te0585(nomopt, nomte)
        case (586)
            call te0586(nomopt, nomte)
        case (587)
            call te0587(nomopt, nomte)
        case (588)
            call te0588(nomopt, nomte)
        case (589)
            call te0589(nomopt, nomte)
        case (590)
            call te0590(nomopt, nomte)
        case (591)
            call te0591(nomopt, nomte)
        case (592)
            call te0592(nomopt, nomte)
        case (593)
            call te0593(nomopt, nomte)
        case (594)
            call te0594(nomopt, nomte)
        case (595)
            call te0595(nomopt, nomte)
        case (596)
            call te0596(nomopt, nomte)
        case (597)
            call te0597(nomopt, nomte)
        case (598)
            call te0598(nomopt, nomte)
        case (599)
            call te0599(nomopt, nomte)
        case (600)
            call te0600(nomopt, nomte)
        case default
            call codent(numc, 'D', k8bid)
            call utmess('F', 'CALCUL_27', 1, k8bid)
        end select
999     continue
    end do
!
    call uttcpu('CPU.CALC.3', 'FIN', ' ')
!
end subroutine
