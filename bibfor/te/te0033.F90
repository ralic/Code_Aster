subroutine te0033(option, nomte)
    implicit  none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/coqrep.h"
#include "asterfort/cosiro.h"
#include "asterfort/dkqedg.h"
#include "asterfort/dkqsie.h"
#include "asterfort/dktedg.h"
#include "asterfort/dktsie.h"
#include "asterfort/dsqedg.h"
#include "asterfort/dsqsie.h"
#include "asterfort/dstedg.h"
#include "asterfort/dstsie.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxsiro.h"
#include "asterfort/dxsit2.h"
#include "asterfort/dxsith.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/q4gedg.h"
#include "asterfort/q4gsie.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/t3gedg.h"
#include "asterfort/t3gsie.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpvgl.h"
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
!     CALCUL DE CONTRAINTES, DEFORMATIONS, EFFORTS ET DEFORMATIONS
!     GENERALISES POUR LES ELEMENTS DKT, DKTG, DST, DKQ, DSQ ET Q4G
!     POUR UN MATERIAU ISOTROPE OU MULTICOUCHE
!         OPTIONS TRAITEES  ==>  SIEF_ELGA
!                                EPSI_ELGA
!                                DEGE_ELGA
!                                DEGE_ELNO
!     IN   K16   OPTION : NOM DE L'OPTION A CALCULER
!     IN   K16   NOMTE  : NOM DU TYPE_ELEMENT
!     ------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: i, iret, jcara, vali(2)
    integer :: jdepg, jeffg, jgeom, jmate, jsigm
    integer :: np, multic
    integer :: jnbspi, nbcou, icou
    integer :: icodre, kpg, spt
!
    real(kind=8) :: zero, epi, epais, eptot, alpha, beta
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), r8bid, valr(2)
    real(kind=8) :: depl(24)
    real(kind=8) :: effgt(32), effpg(32)
    real(kind=8) :: t2iu(4), t2ui(4), c, s
!
    logical :: dkg
!
    character(len=2) :: val
    character(len=3) :: num
    character(len=4) :: fami
    character(len=8) :: nomres, famil, poum
    character(len=16) :: phenom
!     ------------------------------------------------------------------
!
!
    if (option(6:9) .eq. 'ELNO') then
        fami = 'NOEU'
    else
        fami = 'RIGI'
    endif
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
    if (option .ne. 'SIEF_ELGA' .and. option .ne. 'EPSI_ELGA' .and. option .ne. 'DEGE_ELNO'&
        .and. option .ne. 'DEGE_ELGA') then
!C OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
    dkg = .false.
!
    if ((nomte.eq.'MEDKTG3') .or. (nomte.eq.'MEDKQG4')) then
        dkg = .true.
    endif
!
    zero = 0.0d0
!
    do 10 i = 1, 32
        effgt(i) = zero
10  end do
!
    call jevech('PGEOMER', 'L', jgeom)
!
! --- VERIFICATION DE LA COHERENCE DES INFORMATIONS
! --- PROVENANT DE DEFI_COQU_MULT ET DE AFFE_CARA_ELEM
!     ----------------------------------
    jnbspi = 0
    if (option .eq. 'SIEF_ELGA' .or. option .eq. 'EPSI_ELGA') then
        call jevech('PMATERC', 'L', jmate)
        call tecach('NNN', 'PNBSP_I', 'L', 1, jnbspi,&
                    iret)
        if (iret .eq. 0) then
            nbcou = zi(jnbspi)
            icou = 0
            eptot = 0.d0
            epi = 0.d0
            call jevech('PCACOQU', 'L', jcara)
            epais = zr(jcara)
 5          continue
            icou=icou+1
            call codent(icou, 'G', num)
            call codent(1, 'G', val)
            nomres = 'C'//num//'_V'//val
            famil='FPG1'
            kpg=1
            spt=1
            poum='+'
            call rcvalb(famil, kpg, spt, poum, zi(jmate),&
                        ' ', 'ELAS_COQMU', 0, ' ', r8bid,&
                        1, nomres, epi, icodre, 0)
            if (icodre .eq. 0) then
                eptot=eptot+epi
                goto 5
            endif
            if (eptot .ne. 0.d0) then
                if ((icou-1) .ne. nbcou) then
                    vali(1) = icou-1
                    vali(2) = nbcou
                    call u2mesg('F', 'ELEMENTS3_51', 0, ' ', 2,&
                                vali, 0, 0.d0)
                endif
                if (abs(epais-eptot)/epais .gt. 1.d-2) then
                    valr(1) = eptot
                    valr(2) = epais
                    call u2mesg('F', 'ELEMENTS3_52', 0, ' ', 0,&
                                0, 2, valr)
                endif
            endif
        endif
    endif
!
    if (option(8:9) .eq. 'GA') then
        np = npg
    else if (option(8:9).eq.'NO') then
        np = nno
    endif
!
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
!
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
    call jevech('PCACOQU', 'L', jcara)
    alpha = zr(jcara+1) * r8dgrd()
    beta = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2iu, t2ui,&
                c, s)
!
    call jevech('PDEPLAR', 'L', jdepg)
    call utpvgl(nno, 6, pgl, zr(jdepg), depl)
!
!     ---------- CONTRAINTES ET DEFORMATIONS --------------------------
    if (option(1:9) .eq. 'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', jsigm)
!
        if (dkg) then
            nbcou = 1
        else
            call jevech('PNBSP_I', 'L', jnbspi)
            nbcou = zi(jnbspi)
            if (nbcou .le. 0) call u2mess('F', 'ELEMENTS_46')
        endif
!
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDKTG3') then
            call dktsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEDSTR3') then
            call dstsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEDKQU4' .or. nomte.eq.'MEDKQG4') then
            call dkqsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEDSQU4') then
            call dsqsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEQ4QU4') then
            call q4gsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MET3TR3') then
            call t3gsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else
! TYPE D ELEMENT INVALIDE
            ASSERT(.false.)
        endif
!
        call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre)
!        ON NE SAIT PAS TRAITER LE CAS ELAS_COQUE
        if (phenom .eq. 'ELAS' .or. phenom .eq. 'ELAS_ORTH' .or. phenom .eq. 'ELAS_ISTR') then
            call dxsith(nomte, zi(jmate), zr(jsigm))
        else if (phenom.eq.'ELAS_COQMU') then
            call dxsit2(nomte, pgl, zr(jsigm))
        endif
!     ----------------------------
    else if (option(1:9) .eq. 'EPSI_ELGA') then
        call jevech('PDEFOPG', 'E', jsigm)
!
        if (dkg) then
            nbcou = 1
        else
            call jevech('PNBSP_I', 'L', jnbspi)
            nbcou = zi(jnbspi)
            if (nbcou .le. 0) call u2mess('F', 'ELEMENTS_46')
        endif
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDKTG3') then
            call dktsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEDSTR3') then
            call dstsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEDKQU4' .or. nomte.eq.'MEDKQG4') then
            call dkqsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEDSQU4') then
            call dsqsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MEQ4QU4') then
            call q4gsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        else if (nomte.eq.'MET3TR3') then
            call t3gsie(option, fami, xyzl, pgl, depl,&
                        nbcou, zr(jsigm))
        endif
        call dxsiro(np*nbcou*3, t2iu, zr(jsigm), zr(jsigm))
!     ----------------------------
    else if (option(1:9) .eq. 'DEGE_ELNO') then
        call jevech('PDEFOGR', 'E', jeffg)
!
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDKTG3') then
            call dktedg(xyzl, option, pgl, depl, effgt,&
                        multic)
        else if (nomte.eq.'MEDSTR3') then
            call dstedg(xyzl, option, pgl, depl, effgt)
        else if (nomte.eq.'MEDKQU4' .or. nomte.eq.'MEDKQG4') then
            call dkqedg(xyzl, option, pgl, depl, effgt)
        else if (nomte.eq.'MEDSQU4') then
            call dsqedg(xyzl, option, pgl, depl, effgt)
        else if (nomte.eq.'MEQ4QU4'.or. nomte.eq.'MEQ4GG4') then
            call q4gedg(xyzl, option, pgl, depl, effgt)
        else if (nomte.eq.'MET3TR3'.or. nomte.eq.'MET3GG3') then
            call t3gedg(xyzl, option, pgl, depl, effgt)
        endif
!
! ---    PASSAGE DES DEFORMATIONS GENERALISEES DU REPERE INTRINSEQUE
! ---    A L'ELEMENT AU REPERE LOCAL DE LA COQUE
        call dxefro(np, t2iu, effgt, zr(jeffg))
!     ----------------------------
    else if (option(1:9) .eq. 'DEGE_ELGA') then
        call jevech('PDEFOPG', 'E', jeffg)
!
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MEDKTG3') then
            call dktedg(xyzl, option, pgl, depl, effpg,&
                        multic)
        else if (nomte.eq.'MEDSTR3') then
            call dstedg(xyzl, option, pgl, depl, effpg)
        else if (nomte.eq.'MEDKQU4' .or. nomte.eq.'MEDKQG4') then
            call dkqedg(xyzl, option, pgl, depl, effpg)
        else if (nomte.eq.'MEDSQU4') then
            call dsqedg(xyzl, option, pgl, depl, effpg)
        else if (nomte.eq.'MEQ4QU4'.or. nomte.eq.'MEQ4GG4') then
            call q4gedg(xyzl, option, pgl, depl, effpg)
        else if (nomte.eq.'MET3TR3'.or. nomte.eq.'MET3GG3') then
            call t3gedg(xyzl, option, pgl, depl, effpg)
        endif
! ---    PASSAGE DES DEFORMATIONS GENERALISEES DU REPERE INTRINSEQUE
! ---    A L'ELEMENT AU REPERE LOCAL DE LA COQUE
        call dxefro(np, t2iu, effpg, zr(jeffg))
    endif
!
    if (option .eq. 'SIEF_ELGA') then
! ---    PASSAGE DES CONTRAINTES DANS LE REPERE UTILISATEUR :
        call cosiro(nomte, 'PCONTRR', 'E', 'IU', 'G',&
                    jsigm, 'S')
    endif
!
end subroutine
