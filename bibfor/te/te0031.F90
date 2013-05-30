subroutine te0031(option, nomte)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/cosiro.h'
    include 'asterfort/dkqmas.h'
    include 'asterfort/dkqrig.h'
    include 'asterfort/dktmas.h'
    include 'asterfort/dktnli.h'
    include 'asterfort/dktrig.h'
    include 'asterfort/dsqmas.h'
    include 'asterfort/dsqrig.h'
    include 'asterfort/dstmas.h'
    include 'asterfort/dstrig.h'
    include 'asterfort/dxbsig.h'
    include 'asterfort/dxeffi.h'
    include 'asterfort/dxiner.h'
    include 'asterfort/dxqpgl.h'
    include 'asterfort/dxroep.h'
    include 'asterfort/dxtpgl.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/q4gmas.h'
    include 'asterfort/q4grig.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/t3grig.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utpslg.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/utpvlg.h'
    include 'asterfort/vecma.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL DES OPTIONS DES ELEMENTS DE PLAQUE
!          -----------------------------------------------------------
!                                              TRIANGLE  QUADRANGLE
!        LINEAIRE          KIRCHOFF  (MINCE)        DKT       DST
!                 AVEC CISAILLEMENT  (EPAISSE)      DST       DSQ
!                                                   Q4G       T3G
!
!        RIGI_MECA       MASS_MECA
!        EPOT_ELEM  ECIN_ELEM
!        MASS_INER
!          -----------------------------------------------------------
!                                              TRIANGLE
!        LINEAIRE          KIRCHOFF  (MINCE)        DKT
!
!        FORC_NODA
!          -----------------------------------------------------------
!                                              TRIANGLE
!        NON LINEAIRE      KIRCHOFF  (MINCE)        DKT
!
!        FULL_MECA       RAPH_MECA     RIGI_MECA_TANG
!
!
!
    integer :: npge
    parameter(npge=3)
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano, ind
    integer :: multic, codret, jdepm, jdepr
    integer :: icompo, i1, i2, j, jvect, kpg, spt
    integer :: k, jcret, jfreq, iacce
    integer :: jmate, jgeom, jmatr, jener, i, jcara
    integer :: ivect, nddl, nvec, iret, icontp
    integer :: icou, nbcou, jnbspi, iret1, vali(2), itab(7), nbsp
    integer :: ibid, n1, n2, ni
!
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), bsigma(24), effgt(32)
    real(kind=8) :: vecloc(24), ener(3), matp(24, 24), matv(300)
    real(kind=8) :: epi, eptot, r8bid, valr(2)
!
    character(len=2) :: val
    character(len=3) :: num
    character(len=8) :: nomres, fami, poum
    character(len=10) :: phenom
!
    logical :: lcqhom
!
!     ---> POUR DKT/DST MATELEM = 3 * 6 DDL = 171 TERMES STOCKAGE SYME
!     ---> POUR DKQ/DSQ MATELEM = 4 * 6 DDL = 300 TERMES STOCKAGE SYME
    real(kind=8) :: matloc(300), rho, epais
!
!     --->   UML : DEPLACEMENT A L'INSTANT T- (REPERE LOCAL)
!     --->   DUL : INCREMENT DE DEPLACEMENT   (REPERE LOCAL)
    real(kind=8) :: uml(6, 4), dul(6, 4)
!
! DEB ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
! --- PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
    call cosiro(nomte, 'PCONTMR', 'L', 'UI', 'G',&
                ibid, 'S')
    call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G',&
                ibid, 'S')
!
    jnbspi=0
    call tecach('NNN', 'PNBSP_I', 'L', 1, jnbspi,&
                iret1)
!
    lcqhom=.false.
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA' .or. option(1:9) .eq.&
        'RIGI_MECA') then
!
        call jevech('PMATERC', 'L', jmate)
!
! ---   COQUE HOMOGENEISEE ?
!
        if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA' .or. option .eq.&
            'RIGI_MECA_TANG') then
            call rccoma(zi(jmate), 'ELAS', 1, phenom, codret)
!
            if (phenom .eq. 'ELAS_COQUE' .or. phenom .eq. 'ELAS_COQMU') then
                lcqhom=.true.
            endif
        endif
!
! ---   VERIFICATION DE LA COHERENCE DES INFORMATIONS
! ---   PROVENANT DE DEFI_COQU_MULT ET DE AFFE_CARA_ELEM
!       ----------------------------------
        fami='FPG1'
        kpg=1
        spt=1
        poum='+'
        if (iret1 .eq. 0) then
            nbcou=zi(jnbspi)
            icou=0
            eptot=0.d0
            epi=0.d0
            call jevech('PCACOQU', 'L', jcara)
            epais=zr(jcara)
10          continue
            icou=icou+1
            call codent(icou, 'G', num)
            call codent(1, 'G', val)
            nomres='C'//num//'_V'//val
            call rcvalb(fami, kpg, spt, poum, zi(jmate),&
                        ' ', 'ELAS_COQMU', 0, ' ', r8bid,&
                        1, nomres, epi, codret, 0)
            if (codret .eq. 0) then
                eptot=eptot+epi
                goto 10
            endif
            if (eptot .ne. 0.d0) then
                if ((icou-1) .ne. nbcou) then
                    vali(1)=icou-1
                    vali(2)=nbcou
                    call u2mesg('F', 'ELEMENTS3_51', 0, ' ', 2,&
                                vali, 0, 0.d0)
                endif
                if (abs(epais-eptot)/epais .gt. 1.d-2) then
                    valr(1)=eptot
                    valr(2)=epais
                    call u2mesg('F', 'ELEMENTS3_52', 0, ' ', 0,&
                                0, 2, valr)
                endif
            endif
        endif
    endif
!
    call jevech('PGEOMER', 'L', jgeom)
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
    if (option .eq. 'RIGI_MECA' .or. option .eq. 'EPOT_ELEM') then
!     --------------------------------------
!
        if (nomte .eq. 'MEDKTR3') then
            call dktrig(nomte, xyzl, option, pgl, matloc,&
                        ener, multic)
        else if (nomte.eq.'MEDSTR3') then
            call dstrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEDKQU4') then
            call dkqrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEDSQU4') then
            call dsqrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEQ4QU4') then
            call q4grig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MET3TR3') then
            call t3grig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        endif
!
        if (option .eq. 'RIGI_MECA') then
            call jevech('PMATUUR', 'E', jmatr)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
!
        else if (option.eq.'EPOT_ELEM') then
            call jevech('PENERDR', 'E', jener)
            do 60 i = 1, 3
                zr(jener-1+i)=ener(i)
60          continue
        endif
!
!
        elseif (option.eq.'MASS_MECA' .or. option.eq.'MASS_MECA_DIAG'&
    .or. option.eq.'MASS_MECA_EXPLI' .or. option.eq.'M_GAMMA' .or.&
    option.eq.'ECIN_ELEM') then
!     ------------------------------------------
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MET3TR3') then
            call dktmas(xyzl, option, pgl, matloc, ener,&
                        multic)
        else if (nomte.eq.'MEDSTR3') then
            call dstmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEDKQU4') then
            call dkqmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEDSQU4') then
            call dsqmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEQ4QU4') then
            call q4gmas(xyzl, option, pgl, matloc, ener)
        endif
        if (option .eq. 'MASS_MECA') then
            call jevech('PMATUUR', 'E', jmatr)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
        else if (option.eq.'ECIN_ELEM') then
            call jevech('PENERCR', 'E', jener)
            call jevech('POMEGA2', 'L', jfreq)
            do 70 i = 1, 3
                zr(jener-1+i)=zr(jfreq)*ener(i)
70          continue
        else if (option.eq.'M_GAMMA') then
            call jevech('PACCELR', 'L', iacce)
            call jevech('PVECTUR', 'E', ivect)
            nddl=6*nno
            nvec=nddl*(nddl+1)/2
            call utpslg(nno, 6, pgl, matloc, matv)
            call vecma(matv, nvec, matp, nddl)
            call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
            elseif (option.eq.'MASS_MECA_DIAG' .or.&
     &           option.eq.'MASS_MECA_EXPLI') then
            call jevech('PMATUUR', 'E', jmatr)
            nddl=6*nno
            ndim=nddl*(nddl+1)/2
            do 80 i = 1, ndim
                zr(jmatr-1+i)=matloc(i)
80          continue
            if (option .eq. 'MASS_MECA_EXPLI') then
!     CORRECTION DES TERMES CORRESPONDANT AU DDL 6
!     NON PREVU PAR LA THEORIE DKT. ON RAJOUTE
!     UN TERME DIAGONAL NON ZERO EGAL A CELUI DU DDL 5.
!     CETTE CORRECTION A ETE INSPIRE PAR LA DEMARCHE DANS EUROPLEXUS
                do 90 j = 1, nno
                    n1=6*(j-1)+5
                    n2=6*(j-1)+4
                    ni=6*j
                    ndim=(ni+1)*ni/2
                    n1=(n1+1)*n1/2
                    n2=(n2+1)*n2/2
                    zr(jmatr-1+ndim)=(zr(jmatr-1+n1)+zr(jmatr-1+n2))*&
                    0.5d0
90              continue
            endif
        endif
!
!
    else if (option.eq.'MASS_INER') then
!     -----------------------------------
        call jevech('PMASSINE', 'E', jmatr)
        call dxroep(rho, epais)
        call dxiner(nno, zr(jgeom), rho, epais, zr(jmatr),&
                    zr(jmatr+1), zr(jmatr+4))
!
!     -- OPTIONS NON-LINEAIRES :
!     --------------------------
        elseif (option(1:9).eq.'FULL_MECA'.or. option.eq.'RAPH_MECA'.or.&
    option(1:10).eq.'RIGI_MECA_') then
!
        call jevech('PDEPLMR', 'L', jdepm)
        call jevech('PDEPLPR', 'L', jdepr)
        call jevech('PCOMPOR', 'L', icompo)
        if (zk16(icompo+3) .eq. 'COMP_ELAS') then
            call u2mess('F', 'ELEMENTS2_71')
        endif
        if (lcqhom) then
            call u2mess('F', 'ELEMENTS2_75')
        endif
        if ((zk16(icompo+2)(6:10).eq.'_REAC') .or. (zk16(icompo+2) .eq.'GROT_GDEP')) then
!            GROT_GDEP CORRESPOND ICI A EULER_ALMANSI
            if (zk16(icompo+2)(6:10) .eq. '_REAC') then
                call u2mess('A', 'ELEMENTS2_72')
            endif
            do 100 i = 1, nno
                i1=3*(i-1)
                i2=6*(i-1)
                zr(jgeom+i1) = zr(jgeom+i1) + zr(jdepm+i2) + zr(jdepr+ i2)
                zr(jgeom+i1+1)= zr(jgeom+i1+1) + zr(jdepm+i2+1) +&
                zr(jdepr+i2+1)
                zr(jgeom+i1+2)= zr(jgeom+i1+2) + zr(jdepm+i2+2) +&
                zr(jdepr+i2+2)
100          continue
!
            if (nno .eq. 3) then
                call dxtpgl(zr(jgeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(jgeom), pgl, 'S', iret)
            endif
            call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
        endif
!
        call utpvgl(nno, 6, pgl, zr(jdepm), uml)
        call utpvgl(nno, 6, pgl, zr(jdepr), dul)
!
        if (nomte .eq. 'MEDKTR3') then
            call dktnli(nomte, option, xyzl, uml, dul,&
                        vecloc, matloc, codret)
        else if (nomte.eq.'MEDKQU4 ') then
            call dktnli(nomte, option, xyzl, uml, dul,&
                        vecloc, matloc, codret)
        else
            call u2mesk('F', 'ELEMENTS2_74', 1, nomte)
        endif
!
        if (option(1:9) .eq. 'FULL_MECA') then
            call jevech('PMATUUR', 'E', jmatr)
            call jevech('PVECTUR', 'E', jvect)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
            call utpvlg(nno, 6, pgl, vecloc, zr(jvect))
        else if (option.eq.'RAPH_MECA') then
            call jevech('PVECTUR', 'E', jvect)
            call utpvlg(nno, 6, pgl, vecloc, zr(jvect))
        else if (option(1:10).eq.'RIGI_MECA_') then
            call jevech('PMATUUR', 'E', jmatr)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
        endif
!
!
    else if (option.eq.'FORC_NODA') then
!     -------------------------------------
        call tecach('OOO', 'PCONTMR', 'L', 7, itab,&
                    iret)
        icontp=itab(1)
        nbsp=itab(7)
        nbcou=zi(jnbspi)
!
        if (nbsp .ne. npge*nbcou) call u2mess('F', 'ELEMENTS_4')
!
        ind=8
        call dxeffi(option, nomte, pgl, zr(icontp), ind,&
                    effgt)
!
! ------ CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
        call dxbsig(nomte, xyzl, pgl, effgt, bsigma)
!
! ------ AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
        call jevech('PVECTUR', 'E', jvect)
        k=0
        do 120 i = 1, nno
            do 110 j = 1, 6
                k=k+1
                zr(jvect+k-1)=bsigma(k)
110          continue
120      continue
    else
!C OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret)=codret
    endif
!
! --- PASSAGE DES CONTRAINTES DANS LE REPERE UTILISATEUR :
    call cosiro(nomte, 'PCONTPR', 'E', 'IU', 'G',&
                ibid, 'R')
end subroutine
