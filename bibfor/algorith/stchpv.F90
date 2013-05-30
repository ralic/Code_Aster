subroutine stchpv(nbobst, nbpt, temps, dloc, fcho,&
                  vgli, iadh, wk1, wk2, wk3,&
                  iwk4, tdebut, tfin, nbloc, offset,&
                  noecho, intitu, nomres)
    implicit none
    include 'asterc/r8rddg.h'
    include 'asterfort/comptv.h'
    include 'asterfort/dstapv.h'
    include 'asterfort/fstapv.h'
    include 'asterfort/impc0.h'
    include 'asterfort/impdep.h'
    include 'asterfort/impfn0.h'
    include 'asterfort/impftv.h'
    include 'asterfort/infniv.h'
    include 'asterfort/statpu.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'blas/dcopy.h'
    integer :: nbobst, nbpt, nbloc, iadh(*), iwk4(*)
    real(kind=8) :: temps(*), dloc(*), fcho(*), vgli(*), wk1(*), wk2(*), wk3(*)
    real(kind=8) :: tdebut, tfin, offset
    character(len=*) :: nomres
    character(len=8) :: noecho(*), intitu(*)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CALCUL ET IMPRESSION DES STATISTIQUES DE CHOC
!
!     NBOBST       : NB DE NOEUDS DE CHOC
!     NBPT         : NB DE PAS DE TEMPS TEMPORELS ARCHIVES
!     NBLOC        : NB DE BLOCS POUR LE MOYENNAGE
!     TEMPS        : INSTANTS DE CALCUL
!     DLOC         : VECTEUR DES DEPLACEMENTS DANS LE REPERE LOCAL
!     FCHO         : VECTEUR DES FORCES DE CHOC
!     VGLI         : VECTEUR DES VITESSES DE GLISSEMENT
!     IADH         : VECTEUR INDICATEUR D ADHERENCE
!-----------------------------------------------------------------------
    integer :: ibid, valei(3), nbpara, ndepl, nusur, nforn, nstch
!-----------------------------------------------------------------------
    integer :: i, ibl, idebut, idec, ifin, ifires, impr
    integer :: in, indic, j, nbchoc, nbchot, nbmafn, nbmifn
    integer :: nbpas, nbrebo, nbrebt, nbval, nrepc
    real(kind=8) :: dmax, dmin, dxetyp, dxetyt, dxmax, dxmaxt, dxmin
    real(kind=8) :: dxmint, dxmoy, dxmoyt, dxrms, dxrmst, fetyp, fnmax
    real(kind=8) :: fnmaxm, fnmin, fnminm, fnmoyc, fnmoyt, fnrmsc, fnrmst
    real(kind=8) :: frms, ftetyp, ftmax, ftmin, ftmoy, ftmoye, ftrms
    real(kind=8) :: fxmaxt, fxmint, fxmoyc, fxmoyt, fxrmsc, fxrmst, fyetyc
    real(kind=8) :: fyetyt, fymaxt, fymint, fymoyc, fymoyt, fyrmsc, fyrmst
    real(kind=8) :: pusurn, rad, sfn, sfn2, tchmax, tchmin
    real(kind=8) :: tchoc, tchocg, tchocm, tchoct, tchoma, tchomi, tchomy
    real(kind=8) :: trebmy, trebog, trebom, trebot, ttot, txchoc, xtetyp
    real(kind=8) :: xtmoy, xtrms, zero
!-----------------------------------------------------------------------
    parameter     ( nbpara=20, ndepl=8, nusur=4, nforn=8, nstch=10 )
    real(kind=8) :: para(7)
    character(len=8) :: noeud, tpara(nbpara)
    character(len=16) :: tdepl(ndepl), tforn(nforn), tstch(nstch), tusur(nusur)
    character(len=16) :: tvar(10), npara(nbpara), valek(3)
    complex(kind=8) :: c16b
    data tvar  / 'DEPL_X' , 'DEPL_Y' , 'DEPL_Z' , 'DEPL_RADIAL',&
     &             'DEPL_ANGULAIRE' , 'FORCE_NORMALE' , 'FORCE_TANG_1',&
     &             'FORCE_TANG_2' , 'STAT_CHOC' , 'PUIS_USURE' /
    data npara / 'INTITU','NOEUD', 'CALCUL'        , 'MOYEN'        ,&
     &             'ECART_TYPE'    , 'RMS'           , 'MAXI'         ,&
     &             'MINI'          , 'MOYEN_T_TOTAL' , 'MOYEN_T_CHOC' ,&
     &             'RMS_T_TOTAL'   , 'RMS_T_CHOC'    , 'NB_CHOC_S'    ,&
     &             'NB_REBON_CHOC' , 'T_CHOC_MOYEN'  , 'T_CHOC_MAXI'  ,&
     &             'T_CHOC_MINI'   , 'T_REBON_MOYEN' , '%_T_CHOC'     ,&
     &             'PUIS_USURE'    /
    data tpara / 'K8', 'K8'      , 'K16'           , 'R'            ,&
     &             'R'             , 'R'             , 'R'            ,&
     &             'R'             , 'R'             , 'R'            ,&
     &             'R'             , 'R'             , 'I'            ,&
     &             'I'             , 'R'             , 'R'            ,&
     &             'R'             , 'R'             , 'I'            ,&
     &             'R'             /
    data tdepl / 'INTITU','NOEUD', 'CALCUL'        , 'MOYEN'        ,&
     &             'ECART_TYPE'    , 'RMS'           , 'MAXI'         ,&
     &             'MINI'          /
    data tforn / 'INTITU','NOEUD', 'CALCUL'        , 'MOYEN_T_TOTAL',&
     &             'MOYEN_T_CHOC'  , 'RMS_T_TOTAL'   , 'RMS_T_CHOC'   ,&
     &             'MAXI'          /
    data tstch / 'INTITU','NOEUD', 'CALCUL'        , 'NB_CHOC_S'    ,&
     &             'NB_REBON_CHOC' , 'T_CHOC_MOYEN'  , 'T_CHOC_MAXI'  ,&
     &             'T_CHOC_MINI'   , 'T_REBON_MOYEN' , '%_T_CHOC'     /
    data tusur / 'INTITU','NOEUD', 'CALCUL'        , 'PUIS_USURE'   /
!-----------------------------------------------------------------------
!
    zero = 0.0d0
    rad = r8rddg()
    call infniv(ifires, impr)
!
    do 1 i = 1, nbpt
        if (tdebut .le. temps(i)) then
            idebut = i
            goto 2
        endif
 1  end do
 2  continue
    if (tfin .ge. temps(nbpt)) then
        ifin = nbpt
        goto 4
    endif
    do 3 i = 1, nbpt
        if (temps(i) .ge. tfin) then
            ifin = i
            goto 4
        endif
 3  end do
 4  continue
!
    nbpas = ifin - idebut + 1
    if (nbloc .eq. 0) nbloc = 1
    nbval = nbpas / nbloc
    ttot = temps((nbval*nbloc)+idebut-1) - temps(idebut)
!
    if (impr .eq. 2) then
        write(ifires,1000)
        write(ifires,1010) nbloc, nbval
        write(ifires,1000)
        do 5 i = 1, nbloc
            write(ifires,1020) i, temps(idebut+nbval*(i-1)), temps(&
            idebut+nbval*i-1)
 5      continue
    endif
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpara, npara, tpara)
!
    do 10 i = 1, nbobst
        noeud = noecho(i)
        valek(1) = intitu(i)
        valek(2) = noeud
!
        if (impr .eq. 2) then
            write(ifires,*) '   '
            write(ifires,1030)
            write(ifires,1040)
            write(ifires,1050)
            write(ifires,1060) noeud
            write(ifires,1040)
            write(ifires,1030)
        endif
!
!       --------------------------------
!       --- ANALYSE DES DEPLACEMENTS ---
!       --------------------------------
!
        do 11 j = 1, 3
!
            valek(3) = tvar(j)
            dxmoyt = zero
            dxetyt = zero
            dxrmst = zero
            dxmaxt = -1.d30
            dxmint = -dxmaxt
!
            idec=3*(i-1)+j
            call dcopy(nbpt, dloc(idec), 3*nbobst, wk1(1), 1)
!
            do 30 ibl = 1, nbloc
                dxmoy = zero
                dxetyp = zero
                dxrms = zero
                xtmoy = zero
                xtetyp = zero
                xtrms = zero
                dxmax = -1.d30
                dxmin = -dxmax
!
                call dstapv(nbval, wk1((ibl-1)*nbval+idebut), temps(( ibl-1)*nbval+idebut), dmin,&
                            dmax, dxmoy, dxetyp, dxrms, xtmoy,&
                            xtetyp, xtrms)
                dxmoyt = dxmoyt + xtmoy
                dxetyt = dxetyt + xtetyp
                dxrmst = dxrmst + xtrms
                dxmax = max( dxmax , dmax )
                dxmaxt = max( dxmaxt , dxmax )
                dxmin = min( dxmin , dmin )
                dxmint = min( dxmint , dxmin )
                if (impr .eq. 2) call impdep(ifires, j, ibl, dxmoy, dxetyp,&
                                             dxrms, dxmax, dxmin)
30          continue
            dxmoyt = dxmoyt / ttot
            dxrmst = sqrt( dxrmst / ttot )
            dxetyt = sqrt( dxetyt / ttot )
            if (ibl .gt. 1) then
                ibl = 0
                if (impr .eq. 2) call impdep(ifires, j, ibl, dxmoyt, dxetyt,&
                                             dxrmst, dxmaxt, dxmint)
            endif
            para(1) = dxmoyt
            para(2) = dxetyt
            para(3) = dxrmst
            para(4) = dxmaxt
            para(5) = dxmint
            call tbajli(nomres, ndepl, tdepl, ibid, para,&
                        c16b, valek, 0)
11      continue
!
!       --------------------------------------------------------
!       --- ANALYSE DES DEPLACEMENTS EN COORDONNEES POLAIRES ---
!       --------------------------------------------------------
!
        call dcopy(nbpt, dloc(3*(i-1)+2), 3*nbobst, wk1, 1)
        call dcopy(nbpt, dloc(3*(i-1)+3), 3*nbobst, wk2, 1)
        do 12 in = 1, nbpt
            wk3(in) = sqrt( wk1(in)*wk1(in) + wk2(in)*wk2(in) )
12      continue
!
        dxmoyt = zero
        dxetyt = zero
        dxrmst = zero
        xtmoy = zero
        xtetyp = zero
        xtrms = zero
        dxmaxt = -1.d30
        dxmint = -dxmaxt
        do 13 ibl = 1, nbloc
            dxmoy = zero
            dxetyp = zero
            dxrms = zero
            dxmax = -1.d30
            dxmin = -dxmax
            call dstapv(nbval, wk3((ibl-1)*nbval+idebut), temps((ibl- 1)*nbval+idebut), dmin,&
                        dmax, dxmoy, dxetyp, dxrms, xtmoy,&
                        xtetyp, xtrms)
            dxmoyt = dxmoyt + xtmoy
            dxetyt = dxetyt + xtetyp
            dxrmst = dxrmst + xtrms
            dxmin = min(dxmin,dmin)
            dxmax = max(dxmax,dmax)
            dxmaxt = max(dxmaxt,dxmax)
            dxmint = min(dxmint,dxmin)
            if (impr .eq. 2) call impdep(ifires, 4, ibl, dxmoy, dxetyp,&
                                         dxrms, dxmax, dxmin)
13      continue
        dxmoyt = dxmoyt / ttot
        dxrmst = sqrt( dxrmst / ttot )
        dxetyt = sqrt( dxetyt / ttot )
        if (ibl .gt. 1) then
            ibl = 0
            if (impr .eq. 2) call impdep(ifires, 4, ibl, dxmoyt, dxetyt,&
                                         dxrmst, dxmaxt, dxmint)
        endif
        valek(3) = tvar(4)
        para(1) = dxmoyt
        para(2) = dxetyt
        para(3) = dxrmst
        para(4) = dxmaxt
        para(5) = dxmint
        call tbajli(nomres, ndepl, tdepl, ibid, para,&
                    c16b, valek, 0)
!
!       ----------------------------------
!       --- ANALYSE DE L ANGLE POLAIRE ---
!       ----------------------------------
!
        do 14 in = 1, nbpt
            if ((wk1(in).ne.zero) .or. (wk2(in).ne.zero)) then
                wk3(in) = rad*atan2(wk2(in),wk1(in))
            else
                wk3(in) = zero
            endif
14      continue
!
        dxmoyt = zero
        dxetyt = zero
        dxrmst = zero
        xtmoy = zero
        xtetyp = zero
        xtrms = zero
        dxmaxt = -1.d30
        dxmint = -dxmaxt
        do 15 ibl = 1, nbloc
            dxmoy = zero
            dxetyp = zero
            dxrms = zero
            dxmax = -1.d30
            dxmin = -dxmax
            call dstapv(nbval, wk3((ibl-1)*nbval+idebut), temps((ibl- 1)*nbval+idebut), dmin,&
                        dmax, dxmoy, dxetyp, dxrms, xtmoy,&
                        xtetyp, xtrms)
            dxmoyt = dxmoyt + xtmoy
            dxetyt = dxetyt + xtetyp
            dxrmst = dxrmst + xtrms
            dxmin = min(dxmin,dmin)
            dxmax = max(dxmax,dmax)
            dxmaxt = max(dxmaxt,dxmax)
            dxmint = min(dxmint,dxmin)
            if (impr .eq. 2) call impdep(ifires, 5, ibl, dxmoy, dxetyp,&
                                         dxrms, dxmax, dxmin)
15      continue
        dxmoyt = dxmoyt / ttot
        dxrmst = sqrt( dxrmst / ttot )
        dxetyt = sqrt( dxetyt / ttot )
        if (ibl .gt. 1) then
            ibl=0
            if (impr .eq. 2) call impdep(ifires, 5, ibl, dxmoyt, dxetyt,&
                                         dxrmst, dxmaxt, dxmint)
        endif
        valek(3) = tvar(5)
        para(1) = dxmoyt
        para(2) = dxetyt
        para(3) = dxrmst
        para(4) = dxmaxt
        para(5) = dxmint
        call tbajli(nomres, ndepl, tdepl, ibid, para,&
                    c16b, valek, 0)
!
!       ------------------------------------------------------------
!       CALCUL DE LA MOYENNE,ECART TYPE,RMS, MAX DE LA FORCE NORMALE
!       ------------------------------------------------------------
!
        fxmoyt = zero
        fxmoyc = zero
        fxrmst = zero
        fxrmsc = zero
        fxmaxt = zero
        fxmint = zero
        txchoc = zero
        call dcopy(nbpt, fcho(3*(i-1)+1), 3*nbobst, wk1, 1)
        do 22 ibl = 1, nbloc
            fnmoyt = zero
            fnmoyc = zero
            fnrmst = zero
            fnrmsc = zero
            sfn = zero
            sfn2 = zero
            tchoc = zero
            fnmax = -1.d30
            fnmin = -fnmax
            fnminm = fnmin
            fnmaxm = fnmax
            nbmafn = 0
            nbmifn = 0
            call fstapv(nbval, wk1((ibl-1)*nbval+idebut), temps((ibl-1) *nbval+idebut), offset,&
                        fnmoyt, fnmoyc, fnrmst, fnrmsc, fnmax,&
                        fnmin, fnmaxm, fnminm, sfn, sfn2,&
                        tchoc, nbmafn, nbmifn)
            fxmoyt = fxmoyt + sfn
            fxrmst = fxrmst + sfn2
            txchoc = txchoc + tchoc
            fxmaxt = max(fxmaxt,fnmax)
            fxmint = min(fxmint,fnmin)
            if (impr .eq. 2) call impfn0(ifires, ibl, fnmoyt, fnmoyc, fnrmst,&
                                         fnrmsc, fnmax)
22      continue
        fxmoyt = fxmoyt / ttot
        fxrmst = sqrt( fxrmst / ttot )
        if (txchoc .ne. zero) then
            fxmoyc = fxmoyt / txchoc
            fxrmsc = sqrt( fxrmst / txchoc )
        else
            fxmoyc = zero
            fxrmsc = zero
        endif
        if (ibl .gt. 1 .and. impr .eq. 2) call impfn0(ifires, 0, fxmoyt, fxmoyc, fxrmst,&
                                                      fxrmsc, fxmaxt)
        valek(3) = tvar(6)
        para(1) = fxmoyt
        para(2) = fxmoyc
        para(3) = fxrmst
        para(4) = fxrmsc
        para(5) = fxmaxt
        call tbajli(nomres, nforn, tforn, ibid, para,&
                    c16b, valek, 0)
!
!       ----------------------------------------------------------------
!       CALCUL DE LA MOYENNE,ECART TYPE,RMS,MAX DE LA FORCE TANGENTIELLE
!       ----------------------------------------------------------------
!
        do 40 j = 2, 3
            fymoyt = zero
            fyetyt = zero
            fyrmst = zero
            fymoyc = zero
            fyetyc = zero
            fyrmsc = zero
            fymaxt = zero
            fymint = zero
            call dcopy(nbpt, fcho(3*(i-1)+j), 3*nbobst, wk1, 1)
            do 23 ibl = 1, nbloc
                ftmoye = zero
                ftetyp = zero
                ftrms = zero
                ftmoy = zero
                fetyp = zero
                frms = zero
                ftmax = -1.d30
                ftmin = -ftmax
                call dstapv(nbval, wk1((ibl-1)*nbval+idebut), temps(( ibl-1)*nbval+idebut),&
                            ftmin, ftmax, ftmoye, ftetyp, ftrms,&
                            ftmoy, fetyp, frms)
                fymoyt = fymoyt+ftmoy
                fyetyt = fyetyt+fetyp
                fyrmst = fyrmst+frms
                fymaxt = max(fymaxt,ftmax)
                fymint = min(fymint,ftmin)
                indic = j-1
                if (impr .eq. 2) call impftv(ifires, indic, ibl, ftmoye, ftetyp,&
                                             ftrms, ftmoye, ftetyp, ftrms, ftmax,&
                                             ftmin)
23          continue
            fymoyt = fymoyt / ttot
            fyetyt = sqrt( fyetyt / ttot )
            fyrmst = sqrt( fyrmst / ttot )
            if (txchoc .ne. zero) then
                fymoyc = fymoyt / txchoc
                fyetyc = sqrt( fyetyt / txchoc )
                fyrmsc = sqrt( fyrmst / txchoc )
            else
                fymoyc = zero
                fyetyc = zero
                fyrmsc = zero
            endif
            if (ibl .gt. 1 .and. impr .eq. 2) call impftv(ifires, indic, 0, fymoyt, fyetyt,&
                                                          fyrmst, fymoyc, fyetyc, fyrmsc, fymaxt,&
                                                          fymint)
            valek(3) = tvar(5+j)
            para(1) = fymoyt
            para(2) = fyetyt
            para(3) = fyrmst
            para(4) = fymaxt
            para(5) = fymint
            call tbajli(nomres, ndepl, tdepl, ibid, para,&
                        c16b, valek, 0)
40      continue
!
!       -------------------------------------------------------
!       --- CALCUL DU NB DE CHOC, DUREE MOYENNE DE CHOC,... ---
!       -------------------------------------------------------
!
        nbchot = 0
        nbrebt = 0
        tchocg = zero
        trebog = zero
        tchomi = zero
        tchoma = zero
        tchomy = zero
        call dcopy(nbpt, fcho(3*(i-1)+1), 3*nbobst, wk1, 1)
        do 24 ibl = 1, nbloc
            tchocm = zero
            tchoct = zero
            trebom = zero
            trebot = zero
            tchmax = zero
            tchmin = zero
            nbchoc = 0
            nbrebo = 0
            call comptv(nbval, wk1((ibl-1)*nbval+idebut), offset, temps((ibl-1)*nbval+idebut),&
                        nbchoc, tchmin, tchmax, tchoct, tchocm,&
                        nbrebo, trebot, trebom)
            tchoma = max(tchoma,tchmax)
            tchomi = min(tchomi,tchmin)
            tchocg = tchocg + tchoct
            trebog = trebog + trebot
            nbchot = nbchot + nbchoc
            nbrebt = nbrebt + nbrebo
            if (impr .eq. 2) call impc0(ifires, ibl, nbchoc, tchocm, tchmax,&
                                        tchmin, nbrebo, trebom, tchoct, temps,&
                                        nbval)
24      continue
        if (nbchot .ne. 0) then
            tchomy = tchocg / nbchot
        else
            tchomy = zero
        endif
        if (nbrebt .ne. 0) then
            trebmy = trebog / nbrebt
        else
            trebmy = zero
        endif
        if (ibl .gt. 1 .and. impr .eq. 2) then
            call impc0(ifires, 0, nbchot, tchomy, tchoma,&
                       tchomi, nbrebt, trebmy, tchocg, temps,&
                       nbloc*nbval)
        endif
        valek(3) = tvar(9)
        if (nbchot .ne. 0) then
            nrepc = nbrebt / nbchot
        else
            nrepc = 0
        endif
        valei(1) = int( nbchot / ttot )
        valei(2) = nrepc
        para(1) = tchomy
        para(2) = tchoma
        para(3) = tchomi
        para(4) = trebmy
        valei(3) = int( 100.d0 * tchocg / ttot )
        call tbajli(nomres, nstch, tstch, valei, para,&
                    c16b, valek, 0)
!
!       --------------------------------------------------------
!       --- CALCUL DE LA PUISSANCE D'USURE AU SENS D'ARCHARD ---
!       --------------------------------------------------------
!
        call statpu(nbobst, nbpt, temps, fcho, vgli,&
                    iadh, wk1, wk2, wk3, iwk4,&
                    idebut, nbloc, nbval, ifires, i,&
                    impr, pusurn)
        valek(3) = tvar(10)
        call tbajli(nomres, nusur, tusur, ibid, pusurn,&
                    c16b, valek, 0)
!
10  end do
!
    1000 format(7x,'---------------------------------------------------')
    1010 format(9x,'STATISTIQUES SUR ',i3,' BLOC(S) DE ',i7,' VALEURS')
    1020 format(8x,1p,'BLOC NO: ',i3,' DE T= ',e12.5,' A T= ',e12.5)
    1030 format(14x,'****************************************')
    1040 format(14x,'*                                      *')
    1050 format(14x,'*        TRAITEMENT STATISTIQUE        *')
    1060 format(14x,'*         NOEUD DE CHOC: ',a8,'      *')
!
end subroutine
