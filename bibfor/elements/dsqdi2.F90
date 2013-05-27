subroutine dsqdi2(xyzl, df, dci, dmf, dfc,&
                  dmc, an, am)
    implicit  none
    include 'jeveux.h'
    include 'asterfort/dsqbfa.h'
    include 'asterfort/dsqbfb.h'
    include 'asterfort/dsqci2.h'
    include 'asterfort/dsxhft.h'
    include 'asterfort/dxhmft.h'
    include 'asterfort/dxqbm.h'
    include 'asterfort/elref5.h'
    include 'asterfort/gquad4.h'
    include 'asterfort/jquad4.h'
    include 'asterfort/mgauss.h'
    real(kind=8) :: xyzl(3, *), df(3, 3), dmc(3, 2), dfc(3, 2), dci(2, 2)
    real(kind=8) :: dmf(3, 3), an(4, 12), am(4, 8)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_20
!.======================================================================
!
!  DSQDI2 -- DETERMINATION DES MATRICES AN ET AM QUI SONT TELLES QUE
!            ALPHA = AN*UN + AM*UM
!            POUR OBTENIR CETTE EXPRESSION ON PASSE PAR LA RELATION:
!            AA*ALPHA = (AW + AB)*UN + AL*UM
!            UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
!            UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
!            FORMELLEMENT :
!
!             |L5 0  0  0|   |L5C5 L5S5|
!    AA = 2/3*|0 L6  0  0| - |L6C6 L6S6|*DCI*(BCA - DFC_T*BFA)
!             |0  0 L7  0|   |L7C7 L7S7|
!             |0  0  0 L8|   |L8C8 L8S8|
!
!           |L5C5 L5S5|
!    AB = - |L6C6 L6S6|*DCI*DFC_T*BFB
!           |L7C7 L7S7|
!           |L8C8 L8S8|
!
!           |L5C5 L5S5|
!    AL = - |L6C6 L6S6|*DCI*DMC_T*BM
!           |L7C7 L7S7|
!           |L8C8 L8S8|
!
!         |-2  L5C5 L5S5   2  L5C5 L5S5   0     0      0   0     0    0|
! AW=-0.5*| 0     0    0  -2  L6C6 L6S6   2  L6C6   L6S6   0     0    0|
!         | 0     0    0   0     0     0 -2  L7C7   L7S7   2 L7C7  L7S7|
!         | 2  L8C8 L8S8   0     0     0  0     0      0  -2 L8C8  L8S8|
!
!         |L5C5 L5S5|
!       + |L6C6 L6S6|*DCI*BCB
!         |L7C7 L7S7|
!         |L8C8 L8S8|
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    XYZL(3,*)      IN    R       COORDONNEES DES NOEUDS DU DSQ
!    DF(3,3)        IN    R       MATRICE DE FLEXION DE HOOKE
!    DCI(2,2)       IN    R       INVERSE DE LA MATRICE DE CISAILLEMENT
!                                 DE HOOKE
!    DMF(3,3)       IN    R       MATRICE DE COUPLAGE
!                                 MEMBRANE-FLEXION DE HOOKE
!    DFC(3,2)       IN    R       MATRICE DE COUPLAGE
!                                 FLEXION-CISAILLEMENT DE HOOKE
!    DMC(3,2)       IN    R       MATRICE DE COUPLAGE
!                                 MEMBRANE-CISAILLEMENT DE HOOKE
!    AN(4,12)       OUT   R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE FLEXION UN
!    AM(4,8)        OUT   R       MATRICE RELIANT LES ROTATIONS ALPHA
!                                 AUX INCONNUES DE MEMBRANE UM
!
! -----  VARIABLES LOCALES
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: nc, i, j, k, ic, int, iret
    real(kind=8) :: qsi, eta, zero, undemi, un, deux, trois, det
    real(kind=8) :: l(4), x(4), y(4)
    real(kind=8) :: hft2(2, 6), dfcbfa(2, 4), hmft2(2, 6)
    real(kind=8) :: dfcbfb(2, 12), dcidfb(2, 12), bfa(3, 4)
    real(kind=8) :: dmctbm(2, 8), ab(4, 12), aw(4, 12), dcidmc(2, 8)
    real(kind=8) :: bfb(3, 12), bm(3, 8), al(4, 8)
    real(kind=8) :: bcb(2, 12), bca(2, 4), bcm(2, 8)
    real(kind=8) :: db(2, 4), dcb(2, 12)
    real(kind=8) :: aa(4, 4), aai(4, 4), caraq4(25), jacob(5)
!     ------------------------------------------------------------------
!
    call elref5('SE2', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
    nc = 4
!
! --- INITIALISATIONS :
!     ===============
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    trois = 3.0d0
!
    do 10 i = 1, 4
        do 10 j = 1, 8
            am(i,j) = zero
            al(i,j) = zero
10      continue
!
    do 20 i = 1, 4
        do 20 j = 1, 12
            an(i,j) = zero
            aw(i,j) = zero
20      continue
!
    call gquad4(xyzl, caraq4)
    l(1) = caraq4( 9)
    l(2) = caraq4(10)
    l(3) = caraq4(11)
    l(4) = caraq4(12)
    x(1) = caraq4(1)
    x(2) = caraq4(2)
    x(3) = caraq4(3)
    x(4) = caraq4(4)
    y(1) = caraq4(5)
    y(2) = caraq4(6)
    y(3) = caraq4(7)
    y(4) = caraq4(8)
!
!===================================================================
! --- DETERMINATION DES MATRICES AN ET AM QUI SONT TELLES QUE      =
! --- ALPHA = AN*UN + AM*UM                                        =
! --- POUR OBTENIR CETTE EXPRESSION ON PASSE PAR LA RELATION :     =
! --- AA*ALPHA = (AW + AB)*UN + AL*UM                              =
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
! ---  FORMELLEMENT                                                =
! ---            |L5 0  0  0|   |L5C5 L5S5|                        =
! ---   AA = 2/3*|0 L6  0  0| - |L6C6 L6S6|*DCI*(BCA - DFC_T*BFA)  =
! ---            |0  0 L7  0|   |L7C7 L7S7|                        =
! ---            |0  0  0 L8|   |L8C8 L8S8|                        =
!===================================================================
!
! --- BOUCLE SUR LES COTES DU QUADRILATERE :
!     ------------------------------------
!
    do 30 ic = 1, nc
!
        do 40 i = 1, 2
            do 40 j = 1, 4
                db(i,j) = zero
40          continue
!
        do 50 i = 1, 2
            do 50 j = 1, 12
                dcidfb(i,j) = zero
                dcb(i,j) = zero
50          continue
!
        do 60 i = 1, 2
            do 60 j = 1, 8
                dcidmc(i,j) = zero
60          continue
!
! ---   INTEGRATION SUR LE COTE COURANT :
!       -------------------------------
        do 70 int = 1, 2
!
! ---       INITIALISATIONS :
!           ---------------
            do 80 i = 1, 2
                do 80 j = 1, 4
                    dfcbfa(i,j) = zero
80              continue
!
            do 90 i = 1, 2
                do 90 j = 1, 8
                    dmctbm(i,j) = zero
90              continue
!
            do 100 i = 1, 2
                do 100 j = 1, 12
                    dfcbfb(i,j) = zero
100              continue
!
! ---       COORDONNEES DU POINT D'INTEGRATION COURANT :
!           ------------------------------------------
            if (ic .eq. 1) then
                qsi = -zr(icoopg-1+ndim*(int-1)+1)
                eta = -zr(ipoids-1+int)
            else if (ic .eq. 2) then
                qsi = zr(ipoids-1+int)
                eta = -zr(icoopg-1+ndim*(int-1)+1)
            else if (ic .eq. 3) then
                qsi = zr(icoopg-1+ndim*(int-1)+1)
                eta = zr(ipoids-1+int)
            else if (ic .eq. 4) then
                qsi = -zr(ipoids-1+int)
                eta = zr(icoopg-1+ndim*(int-1)+1)
            endif
!
            call jquad4(xyzl, qsi, eta, jacob)
!
! ---       CALCUL DE LA MATRICE HFT2 :
!           -------------------------
            call dsxhft(df, jacob(2), hft2)
!
! ---       CALCUL DU PRODUIT HMF.T2 :
!           ------------------------
            call dxhmft(dmf, jacob(2), hmft2)
!
! ---       CALCUL DES MATRICES  [BCB] ET [BCA] QUI SONT TELLES QUE
! ---       D (BETA)/(DQSI*DQSI) = [TB]*BETA + [TA]*ALPHA :
!           ---------------------------------------------
            call dsqci2(qsi, eta, caraq4, hft2, hmft2,&
                        bcb, bca, bcm)
!
! ---      CALCUL DE LA MATRICE BFA AU POINT D'INTEGRATION COURANT
! ---       RELIANT LES COURBURES AUX INCONNUES ALPHA
! ---       (X = BFB*UN + BFA*ALPHA) :
!           -----------------------
            call dsqbfa(qsi, eta, jacob(2), caraq4, bfa)
!
! ---       CALCUL DU PRODUIT DFC_T*BFA :
!           ---------------------------
            do 110 j = 1, 4
                do 110 k = 1, 3
                    dfcbfa(1,j) = dfcbfa(1,j) + dfc(k,1)*bfa(k,j)
                    dfcbfa(2,j) = dfcbfa(2,j) + dfc(k,2)*bfa(k,j)
110              continue
!
! ---       CALCUL DU PRODUIT DCI*(BCA - DFC_T*BFA) :
!           --------------------------------------
            do 120 j = 1, 4
                db(1,j) = db(1,j) + dci(1,1) * (bca(1,j)-dfcbfa(1,j)) + dci(1,2) * (bca(2,j)-dfcb&
                          &fa(2,j))
                db(2,j) = db(2,j) + dci(2,1) * (bca(1,j)-dfcbfa(1,j)) + dci(2,2) * (bca(2,j)-dfcb&
                          &fa(2,j))
120          continue
!
!================================================================
! --- DETERMINATION DE LA MATRICE AB QUI EST TELLE QUE          =
! --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
! ---  FORMELLEMENT                                             =
! ---          |L5C5 L5S5|                                      =
! ---   AB = - |L6C6 L6S6|*DCI*DFC_T*BFB                        =
! ---          |L7C7 L7S7|                                      =
! ---          |L8C8 L8S8|                                      =
!================================================================
!
! ---      CALCUL DE LA MATRICE BFB RELIANT LES COURBURES AUX
! ---      INCONNUES DE FLEXION UN (X = BFB*UN+BFA*ALPHA) :
!          ----------------------------------------------
!CCCC           QSI = ZR(ICOOPG-1+NDIM*(INT-1)+1)
!CCCC           ETA = ZR(ICOOPG-1+NDIM*(INT-1)+2)
!
            call dsqbfb(qsi, eta, jacob(2), bfb)
!
! ---      CALCUL DU PRODUIT DFC_T*BFB :
!          ---------------------------
            do 130 j = 1, 12
                do 130 k = 1, 3
                    dfcbfb(1,j) = dfcbfb(1,j) + dfc(k,1)*bfb(k,j)
                    dfcbfb(2,j) = dfcbfb(2,j) + dfc(k,2)*bfb(k,j)
130              continue
!
! ---      CALCUL DU PRODUIT DCI*DFC_T*BFB :
!          -------------------------------
            do 140 j = 1, 12
                dcidfb(1,j) = dcidfb(1,j) + dci(1,1)*dfcbfb(1,j) + dci(1,2)*dfcbfb(2,j)
                dcidfb(2,j) = dcidfb(2,j) + dci(2,1)*dfcbfb(1,j) + dci(2,2)*dfcbfb(2,j)
140          continue
!
!===================================================================
! ---      DETERMINATION DE LA MATRICE AL QUI EST TELLE QUE        =
! ---      AA*ALPHA = (AW + AB)*UN + AL*UM                         =
! ---      UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  =
! ---      UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)         =
! ---      FORMELLEMENT                                            =
! ---           |L5C5 L5S5|                                        =
! ---      AL = |L6C6 L6S6|*DCI*(BCM-DMC_T*BM)                     =
! ---           |L7C7 L7S7|                                        =
! ---           |L8C8 L8S8|                                        =
!===================================================================
!
! ---     CALCUL DE LA MATRICE BM RELIANT LES DEFORMATIONS MEMBRANAIRES
! ---     AUX DEPLACEMENTS DE MEMBRANE UM (I.E. (UX,UY) ) :
!         -----------------------------------------------
            call dxqbm(qsi, eta, jacob(2), bm)
!
! ---     CALCUL DU TERME BCM-DMC_T*BM :
!         ----------------------------
            do 150 j = 1, 8
                dmctbm(1,j) = dmctbm(1,j) + bcm(1,j)
                dmctbm(2,j) = dmctbm(2,j) + bcm(2,j)
                do 150 k = 1, 3
                    dmctbm(1,j) = dmctbm(1,j) - dmc(k,1)*bm(k,j)
                    dmctbm(2,j) = dmctbm(2,j) - dmc(k,2)*bm(k,j)
150              continue
!
! ---     CALCUL DU PRODUIT DCI*(BCM-DMC_T*BM) :
!         ------------------------------------
            do 160 j = 1, 8
                dcidmc(1,j) = dcidmc(1,j) + dci(1,1)*dmctbm(1,j) + dci(1,2)*dmctbm(2,j)
                dcidmc(2,j) = dcidmc(2,j) + dci(2,1)*dmctbm(1,j) + dci(2,2)*dmctbm(2,j)
160          continue
!
!======================================================================
! --- DETERMINATION DE LA MATRICE AW QUI EST TELLE QUE
! --- AA*ALPHA = (AW + AB)*UN + AL*UM
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
! ---  FORMELLEMENT
!         |-2  L5C5 L5S5   2  L5C5 L5S5   0     0      0   0     0    0|
! AW=-0.5*| 0     0    0  -2  L6C6 L6S6   2  L6C6   L6S6   0     0    0|
!         | 0     0    0   0     0     0 -2  L7C7   L7S7   2 L7C7  L7S7|
!         | 2  L8C8 L8S8   0     0     0  0     0      0  -2 L8C8  L8S8|
!
!         |L5C5 L5S5|
!       + |L6C6 L6S6|*DCI*BCB
!         |L7C7 L7S7|
!         |L8C8 L8S8|
!
! --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
!
!=======================================================================
!
            do 170 j = 1, 12
                dcb(1,j) = dcb(1,j) + dci(1,1)*bcb(1,j) + dci(1,2)* bcb(2,j)
                dcb(2,j) = dcb(2,j) + dci(2,1)*bcb(1,j) + dci(2,2)* bcb(2,j)
170          continue
!
70      continue
!     -------------------------------------------------------------
! --  FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION DU COTE COURANT
!     -------------------------------------------------------------
!
!===================================================================
! --- DETERMINATION DE LA MATRICE AA QUI EST TELLE QUE             =
! --- AA*ALPHA = (AW + AB)*UN + AL*UM                              =
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
! ---  FORMELLEMENT                                                =
! ---            |L5 0  0  0|   |L5C5 L5S5|                        =
! ---   AA = 2/3*|0 L6  0  0| - |L6C6 L6S6|*DCI*(BCA - DFC_T*BFA)  =
! ---            |0  0 L7  0|   |L7C7 L7S7|                        =
! ---            |0  0  0 L8|   |L8C8 L8S8|                        =
!                                                                  =
! --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y                  =
!===================================================================
!
        do 180 j = 1, 4
            aa(ic,j) = - (x(ic) * db(1,j) + y(ic) * db(2,j))*undemi
180      continue
        aa(ic,ic) = aa(ic,ic) + deux/trois * l(ic)
!
!================================================================
! --- DETERMINATION DE LA MATRICE AB QUI EST TELLE QUE          =
! --- AA*ALPHA = (AW + AB)*UN + AL*UM                           =
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)    =
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)           =
! ---  FORMELLEMENT                                             =
! ---          |L5C5 L5S5|                                      =
! ---   AB = - |L6C6 L6S6|*DCI*DFC_T*BFB                        =
! ---          |L7C7 L7S7|                                      =
! ---          |L8C8 L8S8|                                      =
!                                                               =
! --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y               =
!================================================================
!
        do 190 j = 1, 12
            ab(ic,j) = - (x(ic)*dcidfb(1,j) + y(ic)*dcidfb(2,j))* undemi
190      end do
!
!===================================================================
! ---      DETERMINATION DE LA MATRICE AL QUI EST TELLE QUE        =
! ---      AA*ALPHA = (AW + AB)*UN + AL*UM                         =
! ---      UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)  =
! ---      UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)         =
! ---      FORMELLEMENT                                            =
! ---             |L5C5 L5S5|                                      =
! ---      AL = - |L6C6 L6S6|*DCI*DMC_T*BM                         =
! ---             |L7C7 L7S7|                                      =
! ---             |L8C8 L8S8|                                      =
!                                                                  =
! --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y                  =
!===================================================================
!
        do 200 j = 1, 8
            al(ic,j) = (x(ic)*dcidmc(1,j) + y(ic)*dcidmc(2,j))*undemi
200      continue
!
!======================================================================
! --- DETERMINATION DE LA MATRICE AW QUI EST TELLE QUE
! --- AA*ALPHA = (AW + AB)*UN + AL*UM
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)
! ---  FORMELLEMENT
!         |-2  L5C5 L5S5   2  L5C5 L5S5   0     0      0   0     0    0|
! AW=-0.5*| 0     0    0  -2  L6C6 L6S6   2  L6C6   L6S6   0     0    0|
!         | 0     0    0   0     0     0 -2  L7C7   L7S7   2 L7C7  L7S7|
!         | 2  L8C8 L8S8   0     0     0  0     0      0  -2 L8C8  L8S8|
!
!         |L5C5 L5S5|
!       + |L6C6 L6S6|*DCI*BCB
!         |L7C7 L7S7|
!         |L8C8 L8S8|
!
! --- LES LKCK SONT DANS X , LES LKSK SONT DANS Y :
!
!=======================================================================
!
        do 210 j = 1, 12
            aw(ic,j) = (x(ic)*dcb(1,j) + y(ic)*dcb(2,j))*undemi
210      continue
!
30  end do
!     -------------------------------------------
! --  FIN DE LA BOUCLE SUR LES COTES DE L'ELEMENT
!     -------------------------------------------
!
    aw(1,1) = aw(1,1) + un
    aw(1,2) = aw(1,2) - x(1)/deux
    aw(1,3) = aw(1,3) - y(1)/deux
    aw(1,4) = aw(1,4) - un
    aw(1,5) = aw(1,5) - x(1)/deux
    aw(1,6) = aw(1,6) - y(1)/deux
    aw(2,4) = aw(2,4) + un
    aw(2,5) = aw(2,5) - x(2)/deux
    aw(2,6) = aw(2,6) - y(2)/deux
    aw(2,7) = aw(2,7) - un
    aw(2,8) = aw(2,8) - x(2)/deux
    aw(2,9) = aw(2,9) - y(2)/deux
    aw(3,7) = aw(3,7) + un
    aw(3,8) = aw(3,8) - x(3)/deux
    aw(3,9) = aw(3,9) - y(3)/deux
    aw(3,10) = aw(3,10) - un
    aw(3,11) = aw(3,11) - x(3)/deux
    aw(3,12) = aw(3,12) - y(3)/deux
    aw(4,1) = aw(4,1) - un
    aw(4,2) = aw(4,2) - x(4)/deux
    aw(4,3) = aw(4,3) - y(4)/deux
    aw(4,10) = aw(4,10) + un
    aw(4,11) = aw(4,11) - x(4)/deux
    aw(4,12) = aw(4,12) - y(4)/deux
!
!====================================
! ---    INVERSION DE LA MATRICE AA =
!====================================
!
    do 220 i = 1, 4
        do 220 j = 1, 4
            aai(i,j) = zero
220      continue
    do 230 i = 1, 4
        aai(i,i) = un
230  end do
    call mgauss('NFVP', aa, aai, 4, 4,&
                4, det, iret)
!
!===================================================================
! --- DETERMINATION DE LA MATRICE AN QUI EST TELLE QUE             =
! --- ALPHA = AN*UN + AM*UM                                        =
! --- SOIT AN = AAI * (AW + AB)                                    =
! --- UN DESIGNE LES DEPLACEMENTS DE FLEXION (W,BETAX,BETAY)       =
! --- UM DESIGNE LES DEPLACEMENTS DE MEMBRANE (UX,UY)              =
!===================================================================
!
    do 240 i = 1, 4
        do 240 k = 1, 4
            do 240 j = 1, 12
                an(i,j) = an(i,j) + aai(i,k) * (aw(k,j)+ab(k,j))
240          continue
!
!===================================================================
! --- DETERMINATION DE LA MATRICE AM QUI EST TELLE QUE             =
! --- ALPHA = AN*UN + AM*UM                                        =
! --- SOIT AM = AAI*AL                                             =
!===================================================================
!
    do 250 i = 1, 4
        do 250 k = 1, 4
            do 250 j = 1, 8
                am(i,j) = am(i,j) + aai(i,k) * al(k,j)
250          continue
!
end subroutine
