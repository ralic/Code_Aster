subroutine te0041(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdis.h"
#include "asterfort/infted.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/ut2mlg.h"
#include "asterfort/ut2plg.h"
#include "asterfort/utmess.h"
#include "asterfort/utpplg.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpslg.h"
#include "asterfort/vecma.h"
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
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
!        CALCUL DES MATRICES DE RAIDEUR, MASSE, AMORTISSEMENT
!                   POUR  LES ELEMENTS DISCRETS
!
! --- --------------------------------------------------------------
!  IN
!     OPTION : NOM DE L'OPTION A CALCULER
!        POUR DISCRETS : SYMETRIQUES  ET NON-SYMETRIQUES
!           RIGI_MECA   MASS_MECA  MASS_MECA_DIAG  AMOR_MECA
!        POUR DISCRETS : SYMETRIQUES
!           RIGI_MECA_HYST   RIGI_MECA_TANG   RIGI_FLUI_STRU
!           M_GAMMA          MASS_FLUI_STRU   MASS_MECA_EXPLI
!     NOMTE  : NOM DU TYPE_ELEMENT
!           MECA_DIS_T_N      MECA_DIS_T_L
!           MECA_DIS_TR_N     MECA_DIS_TR_L
!           MECA_2D_DIS_T_N   MECA_2D_DIS_T_L
!           MECA_2D_DIS_TR_N  MECA_2D_DIS_TR_L
! --- --------------------------------------------------------------
!
! --- ------------------------------------------------------------------
    integer :: nddlm, nl1, nl2, infodi
    parameter     (nddlm=12,nl1=nddlm*(nddlm+1)/2,nl2=nddlm*nddlm)
!
    real(kind=8) :: pgl(3, 3), matv1(nl1), matv2(nl1), matp(nddlm, nddlm)
    real(kind=8) :: mata1(nl1), mata2(nl1), mata3(nl2), mata4(nl2)
    real(kind=8) :: eta, r8bid, un, zero, valpar, valres(3)
    complex(kind=8) :: hyst, dcmplx
    integer :: icodre(3), kpg, spt
    character(len=8) :: nomres(3), k8bid, fami, poum
    character(len=24) :: valk(2)
    integer :: ibid, itype, irep, nbterm, nno, nc, ndim, nddl, i, iret, j
    integer :: jdr, jdm, lorien, jdc, jma, iacce, ivect
    integer :: ntermx
    parameter     (zero=0.0d0,un=1.0d0,ntermx=144)
    real(kind=8) :: tempo(ntermx)
    logical :: lbid
! --- ------------------------------------------------------------------
    call jemarq()
!
!
    lbid = (nomte(1:9).eq.'MECA_DIS_') .or. (nomte(1:12).eq.'MECA_2D_DIS_')
    ASSERT(lbid)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
!     ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!     LE CODE DU DISCRET
    call infdis('CODE', ibid, r8bid, nomte)
!     LE CODE STOKE DANS LA CARTE
    call infdis('TYDI', infodi, r8bid, k8bid)
    if (infodi .ne. ibid) then
        call utmess('F+', 'DISCRETS_25', sk=nomte)
        call infdis('DUMP', ibid, r8bid, 'F+')
    endif
!
    valpar = 0.0d0
    infodi = 1
    irep = 1
!
    if (option .eq. 'RIGI_MECA') then
        call infdis('SYMK', infodi, r8bid, k8bid)
    else if (option.eq.'MASS_MECA') then
        call infdis('SYMM', infodi, r8bid, k8bid)
    else if (option.eq.'MASS_MECA_DIAG') then
        call infdis('SYMM', infodi, r8bid, k8bid)
    else if (option.eq.'AMOR_MECA') then
        call infdis('SYMA', infodi, r8bid, k8bid)
    else
!       -- POUR LES AUTRES OPTIONS C'EST SYMETRIQUE
        call infdis('SKMA', ibid, r8bid, k8bid)
        if (ibid .ne. 3) then
            valk(1)=option
            valk(2)=nomte
            call utmess('F', 'DISCRETS_32', nk=2, valk=valk)
        endif
    endif
!
! --- INFORMATIONS SUR LES DISCRETS :
!        NBTERM   = NOMBRE DE COEFFICIENTS DANS K
!        NNO      = NOMBRE DE NOEUDS
!        NC       = NOMBRE DE COMPOSANTE PAR NOEUD
!        NDIM     = DIMENSION DE L'ELEMENT
!        ITYPE    = TYPE DE L'ELEMENT
    call infted(nomte, infodi, nbterm, nno, nc,&
                ndim, itype)
!
!     MATRICES SYMETRIQUES
    if (infodi .eq. 1) then
        nddl = nno * nc
        do 5 i = 1, nl1
            mata1(i) = zero
            mata2(i) = zero
            matv1(i) = zero
            matv2(i) = zero
 5      continue
        if (option .eq. 'RIGI_MECA_HYST') then
            call jevech('PRIGIEL', 'L', jdr)
            call jevech('PMATUUC', 'E', jdm)
            call infdis('ETAK', ibid, eta, k8bid)
            hyst = dcmplx(un,eta)
            do 10 i = 1, nbterm
                zc(jdm+i-1) = zr(jdr+i-1) * hyst
10          continue
            goto 999
        endif
!
        call jevech('PCAORIE', 'L', lorien)
        call matrot(zr(lorien), pgl)
        if (option .eq. 'RIGI_MECA' .or. option .eq. 'RIGI_MECA_TANG' .or. option .eq.&
            'RIGI_FLUI_STRU') then
!           DISCRET DE TYPE RAIDEUR
            call infdis('DISK', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_27', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISK', 'L', jdc)
            call infdis('REPK', irep, r8bid, k8bid)
            call jevech('PMATUUR', 'E', jdm)
            elseif ( option.eq.'MASS_MECA' .or.&
     &            option.eq.'MASS_MECA_DIAG' .or.&
     &            option.eq.'MASS_MECA_EXPLI' .or.&
     &            option.eq.'MASS_FLUI_STRU') then
!           DISCRET DE TYPE MASSE
            call infdis('DISM', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_26', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISM', 'L', jdc)
            call infdis('REPM', irep, r8bid, k8bid)
            call jevech('PMATUUR', 'E', jdm)
        else if (option.eq.'AMOR_MECA') then
!           DISCRET DE TYPE AMORTISSEMENT
            call infdis('DISA', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_28', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISA', 'L', jdc)
            call infdis('REPA', irep, r8bid, k8bid)
            call jevech('PMATUUR', 'E', jdm)
            if (ndim .ne. 3) goto 6
            call tecach('ONN', 'PRIGIEL', 'L', iret, iad=jdr)
            if (jdr .eq. 0) goto 6
            call tecach('NNN', 'PMATERC', 'L', iret, iad=jma)
            if ((jma.eq.0) .or. (iret.ne.0)) goto 6
            nomres(1) = 'RIGI_NOR'
            nomres(2) = 'AMOR_NOR'
            nomres(3) = 'AMOR_TAN'
            valres(1) = zero
            valres(2) = zero
            valres(3) = zero
            call utpsgl(nno, nc, pgl, zr(jdr), matv1)
            call rcvala(zi(jma), ' ', 'DIS_CONTACT', 0, ' ',&
                        [valpar], 3, nomres, valres, icodre,&
                        0)
            if (icodre(1) .eq. 0 .and. valres(1) .ne. zero) then
                if (icodre(2) .eq. 0) then
                    mata1(1)=matv1(1)*valres(2)/valres(1)
                endif
                if (icodre(3) .eq. 0) then
                    mata1(3)=matv1(1)*valres(3)/valres(1)
                endif
                mata1(6) = mata1(3)
            endif
            if (nno .eq. 2 .and. nc .eq. 3) then
                mata1(7) = -mata1(1)
                mata1(10) = mata1(1)
                mata1(15) = mata1(3)
                mata1(21) = mata1(3)
                mata1(12) = -mata1(3)
                mata1(18) = -mata1(3)
            else if (nno.eq.2.and.nc.eq.6) then
                mata1(22) = -mata1(1)
                mata1(28) = mata1(1)
                mata1(36) = mata1(3)
                mata1(45) = mata1(3)
                mata1(30) = -mata1(3)
                mata1(39) = -mata1(3)
            endif
 6          continue
        else if (option.eq.'M_GAMMA') then
!           DISCRET DE TYPE MASSE
            call infdis('DISM', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_26', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISM', 'L', jdc)
            call infdis('REPM', irep, r8bid, k8bid)
            call jevech('PACCELR', 'L', iacce)
            call jevech('PVECTUR', 'E', ivect)
        else
! --- ---   OPTION DE CALCUL INVALIDE
            ASSERT(.false.)
        endif
!
        if (ndim .eq. 3) call utpslg(nno, nc, pgl, mata1, mata2)
        if (irep .eq. 1) then
! --- ---   REPERE GLOBAL ==> PAS DE ROTATION ---
            if (option .eq. 'M_GAMMA') then
                call vecma(zr(jdc), nbterm, matp, nddl)
                call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
            else
                do 20 i = 1, nbterm
                    zr(jdm+i-1) = zr(jdc+i-1)
                    if (option .eq. 'AMOR_MECA') then
                        zr(jdm+i-1) = zr(jdm+i-1) + mata2(i)
                    endif
20              continue
            endif
!
        else if (irep.eq.2) then
! --- ---   LOCAL ==> GLOBAL ---
            if (zr(lorien) .eq. 0.d0 .and. zr(lorien+1) .eq. 0.d0 .and. zr(lorien+2) .eq.&
                0.d0) then
! --- --- ---  ANGLES NULS  ===>  PAS DE ROTATION ---
                if (option .eq. 'M_GAMMA') then
                    call vecma(zr(jdc), nbterm, matp, nddl)
                    call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
                else
                    do 30 i = 1, nbterm
                        zr(jdm+i-1) = zr(jdc+i-1)
                        if (option .eq. 'AMOR_MECA') then
                            zr(jdm+i-1) = zr(jdm+i-1) + mata2(i)
                        endif
30                  continue
                endif
            else
! --- --- ---  ANGLES NON NULS  ===>  ROTATION ---
!              CALL MATROT ( ZR(LORIEN) , PGL )
                if (option .eq. 'M_GAMMA') then
                    if (ndim .eq. 3) then
                        call utpslg(nno, nc, pgl, zr(jdc), matv1)
                    else if (ndim.eq.2) then
                        call ut2mlg(nno, nc, pgl, zr(jdc), matv1)
                    endif
                    call vecma(matv1, nbterm, matp, nddl)
                    call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
                else
                    if (ndim .eq. 3) then
                        call utpslg(nno, nc, pgl, zr(jdc), zr(jdm))
                        if (option .eq. 'AMOR_MECA') then
                            do 25 i = 1, nbterm
                                zr(jdm+i-1) = zr(jdm+i-1) + mata2(i)
25                          continue
                        endif
                    else if (ndim.eq.2) then
                        call ut2mlg(nno, nc, pgl, zr(jdc), zr(jdm))
                    endif
                endif
            endif
        endif
!
!     MATRICES NON-SYMETRIQUES
    else
        nddl = nno * nc
        do 7 i = 1, nl2
            mata3(i) = zero
            mata4(i) = zero
 7      continue
!
        call jevech('PCAORIE', 'L', lorien)
        call matrot(zr(lorien), pgl)
        if (option .eq. 'RIGI_MECA') then
!           DISCRET DE TYPE RAIDEUR
            call infdis('DISK', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_27', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISK', 'L', jdc)
            call infdis('REPK', irep, r8bid, k8bid)
            call jevech('PMATUNS', 'E', jdm)
            elseif ( option.eq.'MASS_MECA'.or. option.eq.'MASS_MECA_DIAG')&
        then
!           DISCRET DE TYPE MASSE
            call infdis('DISM', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_26', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISM', 'L', jdc)
            call infdis('REPM', irep, r8bid, k8bid)
            call jevech('PMATUNS', 'E', jdm)
        else if (option.eq.'AMOR_MECA') then
!           DISCRET DE TYPE AMORTISSEMENT
            call infdis('DISA', infodi, r8bid, k8bid)
            if (infodi .eq. 0) then
                call utmess('A+', 'DISCRETS_28', sk=nomte)
                call infdis('DUMP', ibid, r8bid, 'A+')
            endif
            call jevech('PCADISA', 'L', jdc)
            call infdis('REPA', irep, r8bid, k8bid)
            call jevech('PMATUNS', 'E', jdm)
            if (ndim .ne. 3) goto 8
            call tecach('ONN', 'PRIGIEL', 'L', iret, iad=jdr)
            if (jdr .eq. 0) goto 8
            call tecach('NNN', 'PMATERC', 'L', iret, iad=jma)
            if ((jma.eq.0) .or. (iret.ne.0)) goto 8
            nomres(1) = 'RIGI_NOR'
            nomres(2) = 'AMOR_NOR'
            nomres(3) = 'AMOR_TAN'
            valres(1) = zero
            valres(2) = zero
            valres(3) = zero
            call utpsgl(nno, nc, pgl, zr(jdr), matv2)
            call rcvalb(fami, kpg, spt, poum, zi(jma),&
                        ' ', 'DIS_CONTACT', 0, ' ', [valpar],&
                        3, nomres, valres, icodre, 0)
            if (icodre(1) .eq. 0 .and. valres(1) .ne. zero) then
                if (icodre(2) .eq. 0) then
                    mata3(1)=matv2(1)*valres(2)/valres(1)
                endif
                if (icodre(3) .eq. 0) then
                    mata3(3)=matv2(1)*valres(3)/valres(1)
                endif
            endif
            if (nno .eq. 2 .and. nc .eq. 3) then
                mata3(19) = -mata3(1)
                mata3(22) = mata3(1)
                mata3(29) = mata3(3)
                mata3(36) = mata3(3)
                mata3(26) = -mata3(3)
                mata3(33) = -mata3(3)
                mata3(8) = mata3(3)
                mata3(15) = mata3(3)
                mata3(4) = mata3(19)
                mata3(11) = mata3(26)
                mata3(18) = mata3(33)
                mata3(3) = 0.d0
            else if (nno.eq.2.and.nc.eq.6) then
                mata3(73) = -mata3(1)
                mata3(79) = mata3(1)
                mata3(92) = mata3(3)
                mata3(105)= mata3(3)
                mata3(86) = -mata3(3)
                mata3(99) = -mata3(3)
                mata3(7) = mata3(73)
                mata3(20) = mata3(86)
                mata3(33) = mata3(99)
                mata3(14) = mata3(3)
                mata3(27) = mata3(3)
                mata3(3) = 0.d0
            endif
 8          continue
        else
! --- ---   OPTION DE CALCUL INVALIDE
            ASSERT(.false.)
        endif
        if (ndim .eq. 3) call utpplg(nno, nc, pgl, mata3, mata4)
        if (irep .eq. 1) then
! --- ---   REPERE GLOBAL ==> PAS DE ROTATION ---
            do 21 i = 1, nbterm
                tempo(i) = zr(jdc+i-1)
                if (option .eq. 'AMOR_MECA') then
                    tempo(i) = tempo(i) + mata4(i)
                endif
21          continue
        else if (irep.eq.2) then
! --- ---   LOCAL ==> GLOBAL ---
            if (zr(lorien) .eq. 0.d0 .and. zr(lorien+1) .eq. 0.d0 .and. zr(lorien+2) .eq.&
                0.d0) then
! --- --- ---  ANGLES NULS  ===>  PAS DE ROTATION ---
                do 31 i = 1, nbterm
                    tempo(i) = zr(jdc+i-1)
                    if (option .eq. 'AMOR_MECA') then
                        tempo(i) = tempo(i) + mata4(i)
                    endif
31              continue
            else
! --- --- --- ANGLES NON NULS  ===>  ROTATION ---
!              CALL MATROT ( ZR(LORIEN) , PGL )
                if (ndim .eq. 3) then
                    call utpplg(nno, nc, pgl, zr(jdc), tempo)
                    if (option .eq. 'AMOR_MECA') then
                        do 26 i = 1, nbterm
                            tempo(i) = tempo(i) + mata4(i)
26                      continue
                    endif
                else if (ndim.eq.2) then
                    call ut2plg(nno, nc, pgl, zr(jdc), tempo)
                endif
            endif
        endif
!
        do 27 i = 1, nddl
            do 28 j = 1, nddl
                zr(jdm+(i-1)*nddl+j-1)=tempo((j-1)*nddl+i)
28          continue
27      continue
    endif
!
999  continue
!
    call jedema()
end subroutine
