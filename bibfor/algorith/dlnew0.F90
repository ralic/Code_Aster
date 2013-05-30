subroutine dlnew0(result, force0, force1, iinteg, neq,&
                  istoc, iarchi, ifm, nbexci, nondp,&
                  nmodam, lamort, limped, lmodst, imat,&
                  masse, rigid, amort, nchar, nveca,&
                  liad, lifo, modele, mate, carele,&
                  charge, infoch, fomult, numedd, depla,&
                  vitea, accea, dep0, vit0, acc0,&
                  fexte, famor, fliai, depl1, vite1,&
                  acce1, psdel, fammo, fimpe, fonde,&
                  vien, vite, vita1, mltap, a0,&
                  a2, a3, a4, a5, a6,&
                  a7, a8, c0, c1, c2,&
                  c3, c4, c5, nodepl, novite,&
                  noacce, matres, maprec, solveu, criter,&
                  chondp, ener, vitini, vitent, valmod,&
                  basmod, veanec, vaanec, vaonde, veonde,&
                  dt, theta, tempm, temps, iforc2,&
                  tabwk1, tabwk2, archiv, nbtyar, typear,&
                  numrep)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
! ----------------------------------------------------------------------
!     CALCUL MECANIQUE TRANSITOIRE PAR INTEGRATION DIRECTE
!     AVEC METHODES IMPLICITES :                  - THETA-WILSON
!                                                 - NEWMARK
! ----------------------------------------------------------------------
!  IN  : IINTEG    : ENTIER INDIQUANT LA METHODE D'INTEGRATION
!  IN  : NEQ       : NOMBRE D'EQUATIONS
!  IN  : ISTOC     : PILOTAGE DU STOCKAGE DES RESULTATS
!  IN  : IARCHI    : PILOTAGE DE L'ARCHIVAGE DES RESULTATS
!  IN  : NBEXCI    : NOMBRE D'EXCITATIONS
!  IN  : NONDP     : NOMBRE D'ONDES PLANES
!  IN  : NMODAM    : NOMBRE D'AMORTISSEMENTS MODAUX
!  IN  : LAMORT    : LOGIQUE INDIQUANT SI IL Y A AMORTISSEMENT
!  IN  : LIMPED    : LOGIQUE INDIQUANT SI
!  IN  : LMODST    : LOGIQUE INDIQUANT SI MODE STATIQUE
!  IN  : IMAT      : TABLEAU D'ADRESSES POUR LES MATRICES
!  IN  : MASSE     : MATRICE DE MASSE
!  IN  : NCHAR     : NOMBRE D'OCCURENCES DU MOT CLE CHARGE
!  IN  : NVECA     : NOMBRE D'OCCURENCES DU MOT CLE VECT_ASSE
!  IN  : LIAD      : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
!  IN  : LIFO      : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
!  IN  : MODELE    : NOM DU MODELE
!  IN  : MATE      : NOM DU CHAMP DE MATERIAU
!  IN  : CARELE    : CARACTERISTIQUES DES POUTRES ET COQUES
!  IN  : CHARGE    : LISTE DES CHARGES
!  IN  : INFOCH    : INFO SUR LES CHARGES
!  IN  : FOMULT    : LISTE DES FONC_MULT ASSOCIES A DES CHARGES
!  IN  : NUMEDD    : NUME_DDL DE LA MATR_ASSE RIGID
!  IN  : SOLVEU    : NOM DU SOLVEUR
!  IN  : CRITER    :
!  IN  : CHONDP    : NOMS DES ONDES PLANES
!  VAR : DEP0      : TABLEAU DES DEPLACEMENTS A L'INSTANT N
!  VAR : VIT0      : TABLEAU DES VITESSES A L'INSTANT N
!  VAR : ACC0      : TABLEAU DES ACCELERATIONS A L'INSTANT N
!  IN  : DT        : PAS DE TEMPS
!  IN  : THETA     : PARAMETRE DU SCHEMA TEMPOREL
!  IN  : TEMPM     : INSTANT PRECEDENT
!  IN  : TEMPS     : INSTANT COURANT
! IN  NUMREP : NUMERO DE REUSE POUR LA TABLE PARA_CALC
!
! CORPS DU PROGRAMME
    implicit none
! DECLARATION PARAMETRES D'APPELS
!
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dlarch.h'
    include 'asterfort/dlfext.h'
    include 'asterfort/enerca.h'
    include 'asterfort/fimped.h'
    include 'asterfort/fmodam.h'
    include 'asterfort/fointe.h'
    include 'asterfort/fondpl.h'
    include 'asterfort/forcdy.h'
    include 'asterfort/fteta.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/newacc.h'
    include 'asterfort/newdep.h'
    include 'asterfort/newvit.h'
    include 'asterfort/nmarpc.h'
    include 'asterfort/resoud.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/vtcopy.h'
    include 'asterfort/vtcreb.h'
    include 'asterfort/wkvect.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    integer :: nbexci, nondp, nmodam, iinteg, neq
    integer :: istoc, iarchi, ifm, imat(3), nchar, nveca, liad(*)
    integer :: iforc2, archiv, nbtyar, mltap(nbexci)
!
    real(kind=8) :: depla(neq), vitea(neq), accea(neq), rbid, dep0(*), vit0(*)
    real(kind=8) :: acc0(*), fexte(*), famor(*), fliai(*), depl1(neq)
    real(kind=8) :: vite1(neq), acce1(neq), psdel(neq), fammo(neq), fimpe(neq)
    real(kind=8) :: fonde(neq), vien(neq), vite(neq), vita1(neq), a0, a2, a3
    real(kind=8) :: a4, a5, a6, a7, a8, c0, c1, c2, c3, c4, c5, tabwk1(neq)
    real(kind=8) :: tabwk2(neq), dt, theta, tempm, temps
!
    logical :: lamort, limped, lmodst, ener
!
    character(len=8) :: nodepl(nbexci), novite(nbexci), noacce(nbexci)
    character(len=8) :: masse, rigid, amort
    character(len=8) :: result
    character(len=19) :: force0, force1
    character(len=8) :: chondp(nondp)
    character(len=8) :: matres
    character(len=16) :: typear(nbtyar)
    character(len=19) :: solveu
    character(len=19) :: maprec
    character(len=24) :: criter
    character(len=24) :: modele, mate, carele, charge, infoch, fomult, numedd
    character(len=24) :: vitini, vitent, valmod, basmod
    character(len=24) :: lifo(*)
    character(len=24) :: veanec, vaanec, vaonde, veonde
!
!
! DECLARATION VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'DLNEW0' )
!
    integer :: iforc0, iforc1
    integer :: lresu, lcrre, nbexre, item2, iret, lvale, ibid, i
    integer :: lval1, lval2, ltps0, ltps1, nbinst, ifnobi, ifcibi, alarm
    integer :: iexci, ieq, iresu
!
    real(kind=8) :: coefd, coefv, coefa, prec, eps0, alpha
    integer :: numrep
    character(len=8) :: k8bid
    character(len=16) :: typa(6)
    character(len=19) :: chsol, cham19, chamno, chamn2, sdener, k19bid
    character(len=19) :: masse1, amort1, rigid1
    character(len=24) :: cine, veccor, vecond
    complex(kind=8) :: cbid
!     -----------------------------------------------------------------
!
!
! --- NOM DES STRUCTURES DE TRAVAIL
!
    chsol = '&&'//nompro//'.SOLUTION '
    veccor = '&&VECCOR'
    vecond = '&&VECOND'
    cine = '  '
    chamno = '&&'//nompro//'.CHAMNO'
    call jeexin(chamno(1:19)//'.REFE', iret)
    if (iret .eq. 0) then
        call vtcreb(chamno, numedd, 'V', 'R', neq)
    endif
    chamn2 = '&&'//nompro//'.CHAMN2'
    call jeexin(chamn2(1:19)//'.REFE', iret)
    if (iret .eq. 0) then
        call vtcreb(chamn2, numedd, 'V', 'R', neq)
    endif
!====
! 2. DEPLACEMENT, VITESSE ET ACCELERATIONS A
!====
    do 21 , ieq = 1,neq
    depla(ieq) = 0.d0
    vitea(ieq) = 0.d0
    accea(ieq) = 0.d0
    21 end do
!
    if (lmodst) then
!
        do 22 iexci = 1, nbexci
!
            if (mltap(iexci) .eq. 1) then
                call fointe('F ', nodepl(iexci), 1, 'INST', temps,&
                            coefd, ieq)
                call fointe('F ', novite(iexci), 1, 'INST', temps,&
                            coefv, ieq)
                call fointe('F ', noacce(iexci), 1, 'INST', temps,&
                            coefa, ieq)
                do 221 ieq = 1, neq
                    depla(ieq) = depla(ieq) + psdel(ieq)*coefd
                    vitea(ieq) = vitea(ieq) + psdel(ieq)*coefv
                    accea(ieq) = accea(ieq) + psdel(ieq)*coefa
221              continue
            endif
!
22      continue
!
    endif
!
!====
! 3.
!====
    do 31 , ieq = 1,neq
    vite(ieq) = vit0(ieq)
    31 end do
    if (lmodst) then
        do 32 , ieq = 1,neq
        vien(ieq) = vitea(ieq)
32      continue
    endif
    if (limped) then
        call fimped(modele, mate, numedd, neq, vitini,&
                    vitent, veccor, veanec, vaanec, tempm,&
                    fimpe)
    endif
    if (nondp .ne. 0) then
        call fondpl(modele, mate, numedd, neq, chondp,&
                    nondp, vecond, veonde, vaonde, tempm,&
                    fonde)
    endif
!
    if (nmodam .ne. 0) then
        if (lmodst) then
            do 33 , ieq = 1,neq
            vita1(ieq) = vit0(ieq) + vitea(ieq)
33          continue
            call fmodam(neq, vita1, valmod, basmod, fammo)
        else
            call fmodam(neq, vit0, valmod, basmod, fammo)
        endif
    endif
!
!====
! 4. CALCUL DU SECOND MEMBRE F*
!====
    call jeveuo(force0(1:19)//'.VALE', 'E', iforc0)
    call jeveuo(force1(1:19)//'.VALE', 'E', iforc1)
!
    call dlfext(nveca, nchar, temps, neq, liad,&
                lifo, charge, infoch, fomult, modele,&
                mate, carele, numedd, zr(iforc1))
!
    if (nondp .ne. 0) then
        do 43 , ieq = 1,neq
        zr(iforc1+ieq-1) = zr(iforc1+ieq-1) - fonde(ieq)
43      continue
    endif
    if (ener) then
        do 433, ieq =1,neq
        fexte(ieq)=fexte(ieq+neq)
        fexte(ieq+neq)=zr(iforc1+ieq-1)
433      continue
    endif
!
    if (limped) then
        do 41 , ieq = 1,neq
        zr(iforc1+ieq-1) = zr(iforc1+ieq-1) - fimpe(ieq)
41      continue
        if (ener) then
            do 411 ieq = 1, neq
                fliai(ieq)=fliai(ieq+neq)
                fliai(ieq+neq)=fimpe(ieq)
411          continue
        endif
    endif
!
    if (nmodam .ne. 0) then
        do 42 , ieq = 1,neq
        zr(iforc1+ieq-1) = zr(iforc1+ieq-1) - fammo(ieq)
42      continue
        if (ener) then
            do 421 ieq = 1, neq
                famor(ieq)=famor(ieq+neq)
                famor(ieq+neq)=fammo(ieq)
421          continue
        endif
    endif
!
!
!   Chargement venant d'un RESU a TEMPS
!
    call getfac('EXCIT_RESU', nbexre)
    if (nbexre .ne. 0) then
        call jeveuo('&&OP0048.COEF_RRE', 'L', lcrre)
        call jeveuo('&&OP0048.LISTRESU', 'L', lresu)
        prec=1.d-9
        eps0 =1.d-12
        do 210 iresu = 1, nbexre
            if (abs(temps) .gt. eps0) then
                call rsorac(zk8(lresu+iresu-1), 'INST', ibid, temps, k8bid,&
                            cbid, prec, 'RELATIF', item2, 1,&
                            ibid)
            else
                call rsorac(zk8(lresu+iresu-1), 'INST', ibid, temps, k8bid,&
                            cbid, eps0, 'ABSOLU', item2, 1,&
                            ibid)
            endif
            if (ibid .gt. 0) then
                call rsexch('F', zk8(lresu+iresu-1), 'DEPL', item2, cham19,&
                            iret)
                call vtcopy(cham19, chamno, 'F', iret)
                call jeveuo(chamno//'.VALE', 'L', lvale)
!
            else
                call wkvect('&&DLNEW0.XTRAC', 'V V R8', neq, lvale)
                call jelira(zk8(lresu+iresu-1)//'           .ORDR', 'LONUTI', nbinst, k8bid)
!
!        --- INTERPOLATION LINEAIRE ---
                do 211 i = 1, nbinst-1
!
                    call rsadpa(zk8(lresu+iresu-1), 'L', 1, 'INST', i,&
                                0, ltps0, k8bid)
                    call rsadpa(zk8(lresu+iresu-1), 'L', 1, 'INST', i+1,&
                                0, ltps1, k8bid)
                    if (i .eq. 1 .and. temps .lt. zr(ltps0)) then
                        call rsexch('F', zk8(lresu+iresu-1), 'DEPL', i, cham19,&
                                    iret)
                        call vtcopy(cham19, chamno, 'F', iret)
                        call jeveuo(chamno//'.VALE', 'L', lvale)
                        goto 213
                    endif
                    if (temps .ge. zr(ltps0) .and. temps .lt. zr(ltps1)) then
                        alpha = (temps - zr(ltps0)) / (zr(ltps1) - zr( ltps0))
                        call rsexch('F', zk8(lresu+iresu-1), 'DEPL', i, cham19,&
                                    iret)
                        call vtcopy(cham19, chamno, 'F', iret)
                        call jeveuo(chamno//'.VALE', 'L', lval1)
                        call rsexch('F', zk8(lresu+iresu-1), 'DEPL', i+1, cham19,&
                                    iret)
                        call vtcopy(cham19, chamn2, 'F', iret)
                        call jeveuo(chamn2//'.VALE', 'L', lval2)
                        call dcopy(neq, zr(lval1), 1, zr(lvale), 1)
                        call dscal(neq, (1.d0-alpha), zr(lvale), 1)
                        call daxpy(neq, alpha, zr(lval2), 1, zr(lvale),&
                                   1)
                        goto 213
                    endif
                    if (i .eq. nbinst-1 .and. temps .ge. zr(ltps1)) then
                        call rsexch('F', zk8(lresu+iresu-1), 'DEPL', i+1, cham19,&
                                    iret)
                        call vtcopy(cham19, chamno, 'F', iret)
                        call jeveuo(chamno//'.VALE', 'L', lvale)
                        goto 213
                    endif
211              continue
213              continue
            endif
            do 212 ieq = 1, neq
                zr(iforc2+ieq-1) = zr(lvale+ieq-1)*zr(lcrre+iresu-1)
                zr(iforc1+ieq-1) = zr(iforc1+ieq-1) + zr(lvale+ieq-1)* zr(lcrre+iresu-1)
212          continue
            if (ibid .gt. 0) then
                call jelibe(cham19//'.VALE')
            else
                call jelibe(cham19//'.VALE')
                call jedetr('&&DLNEW0.XTRAC')
            endif
210      continue
        if (ener) then
            do 23 ieq = 1, neq
                fexte(ieq+neq)=fexte(ieq+neq)+ zr(iforc2+ieq-1)
23          continue
        endif
    endif
!
    if (iinteg .eq. 2) then
        call dcopy(neq, zr(iforc1), 1, zr(iforc2), 1)
        call fteta(theta, neq, zr(iforc0), zr(iforc1))
    endif
!
!====
! 5. FORCE DYNAMIQUE F*
!====
    call forcdy(imat(2), imat(3), lamort, neq, c0,&
                c1, c2, c3, c4, c5,&
                dep0, vit0, acc0, tabwk1, tabwk2,&
                zr(iforc1))
!
!====
! 6.  RESOLUTION DU PROBLEME K*  . U*  =  P*
!           --- RESOLUTION AVEC FORCE1 COMME SECOND MEMBRE ---
!====
    call resoud(matres, maprec, solveu, cine, 0,&
                force1, chsol, 'V', rbid, cbid,&
                criter, .true., 0, iret)
    call copisd('CHAMP_GD', 'V', chsol(1:19), force1(1:19))
    call jeveuo(force1(1:19)//'.VALE', 'E', iforc1)
    call detrsd('CHAMP_GD', chsol)
    call dcopy(neq, zr(iforc1), 1, depl1, 1)
!
!====
! 7. CALCUL DES DEPLACEMENTS,VITESSES ET ACCELERATIONS
!====
    if (iinteg .eq. 2) then
!
        call newacc(neq, a4, a5, a6, dep0,&
                    vit0, acc0, depl1, acce1)
        call newvit(neq, a7, a7, vit0, acc0,&
                    vite1, acce1)
        call newdep(neq, a8, dt, dep0, vit0,&
                    acc0, depl1, acce1)
!
    else if (iinteg.eq.1) then
!
        call newacc(neq, a0, -a2, -a3, dep0,&
                    vit0, acc0, depl1, acce1)
        call newvit(neq, a6, a7, vit0, acc0,&
                    vite1, acce1)
!
    endif
!
!====
! 8. CALCUL DES ENERGIES
!====
!
    sdener=solveu(1:8)//'.ENER      '
    if (ener) then
        masse1=masse//'           '
        amort1=amort//'           '
        rigid1=rigid//'           '
        call wkvect('FNODABID', 'V V R', 2*neq, ifnobi)
        call wkvect('FCINEBID', 'V V R', 2*neq, ifcibi)
        call enerca(k19bid, dep0, vit0, depl1, vite1,&
                    masse1, amort1, rigid1, fexte, famor,&
                    fliai, zr(ifnobi), zr(ifcibi), lamort, .true.,&
                    .false., sdener, '&&DLNEWI')
        call jedetr('FNODABID')
        call jedetr('FCINEBID')
    endif
!
!====
! 9. TRANSFERT DES NOUVELLES VALEURS DANS LES ANCIENNES
!====
!
    call dcopy(neq, depl1, 1, dep0, 1)
    call dcopy(neq, vite1, 1, vit0, 1)
    call dcopy(neq, acce1, 1, acc0, 1)
!
    if (iinteg .eq. 2) then
        call dcopy(neq, zr(iforc2), 1, zr(iforc0), 1)
    else
        call dcopy(neq, zr(iforc1), 1, zr(iforc0), 1)
    endif
!
!
!====
! 11. ARCHIVAGE EVENTUEL DANS L'OBJET SOLUTION
!====
    if (archiv .eq. 1) then
!
        istoc = 0
        alarm = 1
!
        if (lmodst) then
!
            typa(1) = 'DEPL_ABSOLU'
            typa(2) = 'VITE_ABSOLU'
            typa(3) = 'ACCE_ABSOLU'
            typa(4) = '    '
            typa(5) = '    '
            typa(6) = '    '
            do 101 , ieq = 1,neq
            depla(ieq) = depla(ieq) + dep0(ieq)
            vitea(ieq) = vitea(ieq) + vit0(ieq)
            accea(ieq) = accea(ieq) + acc0(ieq)
101          continue
            call dlarch(result, neq, istoc, iarchi, ' ',&
                        alarm, ifm, temps, nbtyar, typa,&
                        masse, depla, vitea, accea, fexte( neq+1),&
                        famor(neq+1), fliai(neq+1))
!
        endif
!
        call dlarch(result, neq, istoc, iarchi, ' ',&
                    alarm, ifm, temps, nbtyar, typear,&
                    masse, dep0, vit0, acc0, fexte(neq+1),&
                    famor(neq+1), fliai(neq+1))
!
    endif
!
    call nmarpc(result, sdener, numrep, temps)
!
end subroutine
