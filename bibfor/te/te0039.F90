subroutine te0039(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dichoc.h"
#include "asterfort/disief.h"
#include "asterfort/infdis.h"
#include "asterfort/infted.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/r8inir.h"
#include "asterfort/terefe.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utmess.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/lteatt.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! IN OPTION    : K16 :  OPTION DE CALCUL
!      'SIEQ_ELNO'       'SIEQ_ELGA'
!     'FORC_NODA'  'REFE_FORC_NODA'
!     'FONL_NOEU'
! IN NOMTE     : K16 : NOM DU TYPE ELEMENT
!     DISCRETS :
!        'MECA_DIS_T_N'      'MECA_DIS_T_L'     'MECA_DIS_TR_N'
!        'MECA_DIS_TR_L'     'MECA_2D_DIS_T_N'  'MECA_2D_DIS_T_L'
!        'MECA_2D_DIS_TR_N'  'MECA_2D_DIS_TR_L'
!     ------------------------------------------------------------------
    character(len=8) :: k8bid
    character(len=16) :: kmess(5)
!
    real(kind=8) :: zero
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: fs(12)
    real(kind=8) :: r8bid
    real(kind=8) :: ugp(12), dug(12), klv(78), duly, force(3), plouf
    real(kind=8) :: ulp(12), dul(12), dvl(12), dpe(12), dve(12)
    real(kind=8) :: sim(12), sip(12), fono(12), varmo(8), varpl(8)
    real(kind=8) :: forref, momref
!
    integer :: lorien, lmater, in
    integer :: i, ivectu, icontg, neq, nc, nno,ndim
    integer :: iplouf, infodi, itype, ibid
    integer :: igeom, ideplm, ideplp, icompo, nbt, jdc, irep, ifono, ilogic
!
    parameter (zero=0.0d0)
!     ------------------------------------------------------------------
    call jemarq()
    infodi = 1


!   ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!   LE CODE DU DISCRET
    call infdis('CODE', ibid, r8bid, nomte)
!   LE CODE STOKE DANS LA CARTE
    call infdis('TYDI', infodi, r8bid, k8bid)
    if (infodi .ne. ibid) then
        call utmess('F+', 'DISCRETS_25', sk=nomte)
        call infdis('DUMP', ibid, r8bid, 'F+')
    endif
!   DISCRET DE TYPE RAIDEUR
    call infdis('DISK', infodi, r8bid, k8bid)
    if (infodi .eq. 0) then
        call utmess('A+', 'DISCRETS_27', sk=nomte)
        call infdis('DUMP', ibid, r8bid, 'A+')
    endif
!   MATRICE DE RAIDEUR SYMETRIQUE OU PAS, POUR LES DISCRETS
    call infdis('SYMK', infodi, r8bid, k8bid)
!   RECUPERE LES INFORMATIONS SUR LES ELEMENTS
    call infted(nomte, infodi, nbt, nno, nc,&
                ndim, itype)
    neq = nno*nc
!
! --- ------------------------------------------------------------------
    if (option(1:14) .eq. 'REFE_FORC_NODA') then
        call jevech('PVECTUR', 'E', ivectu)
        if (lteatt('CODMOD','DTR')) then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            call terefe('MOMENT_REFE', 'MECA_DISCRET', momref)
            do  in = 1, nno
                do i = 1, 3
                    zr(ivectu+(in-1)*nc+i-1)=forref
                enddo
                do i = 4, nc
                    zr(ivectu+(in-1)*nc+i-1)=momref
                enddo
            enddo
        else if (lteatt('CODMOD','2DT')) then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            do  in = 1, nno
                zr(ivectu+(in-1)*nc)=forref
                zr(ivectu+(in-1)*nc+1)=forref
            enddo
        else if (lteatt('CODMOD','2TR')) then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            call terefe('MOMENT_REFE', 'MECA_DISCRET', momref)
            do in = 1, nno
                zr(ivectu+(in-1)*nc)=forref
                zr(ivectu+(in-1)*nc+1)=forref
                zr(ivectu+(in-1)*nc+2)=momref
            enddo
        else if (lteatt('CODMOD','DIT')) then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            do in = 1, nno
                zr(ivectu+(in-1)*nc)=forref
                zr(ivectu+(in-1)*nc+1)=forref
                zr(ivectu+(in-1)*nc+2)=forref
            enddo
        else
            kmess(1) = option
            kmess(2) = nomte
            kmess(3) = 'TE0039'
            call utmess('F', 'DISCRETS_15', nk=2, valk=kmess)
        endif
! --- ------------------------------------------------------------------
    else if (option.eq.'FONL_NOEU') then
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PMATERC', 'L', lmater)
        if (lteatt('CODMOD','DTR') .or. lteatt('CODMOD','DIT')) then
!           PARAMETRES EN ENTREE
            call jevech('PCAORIE', 'L', lorien)
            call matrot(zr(lorien), pgl)
!           DEPLACEMENTS DANS LE REPERE GLOBAL
!                 UGM = DEPLACEMENT PRECEDENT
!                 DUG = INCREMENT DE DEPLACEMENT
!                 UGP = DEPLACEMENT COURANT
            do 300 i = 1, neq
                dug(i) = zr(ideplp+i-1)
                ugp(i) = zr(ideplm+i-1) + dug(i)
300          continue
!           DEPLACEMENTS DANS LE REPERE LOCAL
!              ULM = DEPLACEMENT PRECEDENT    = PLG * UGM
!              DUL = INCREMENT DE DEPLACEMENT = PLG * DUG
!              ULP = DEPLACEMENT COURANT      = PLG * UGP
            if (ndim .eq. 3) then
                call utpvgl(nno, nc, pgl, dug, dul)
                call utpvgl(nno, nc, pgl, ugp, ulp)
            else if (ndim.eq.2) then
                call ut2vgl(nno, nc, pgl, dug, dul)
                call ut2vgl(nno, nc, pgl, ugp, ulp)
            endif
!           SEUL LE CAS SYMETRIQUE EST TRAITE
            call infdis('SYMK', iplouf, r8bid, k8bid)
            if (iplouf .ne. 1) then
                kmess(1) = option
                kmess(2) = nomte
                kmess(3) = 'TE0039'
                kmess(4) = ' '
                call utmess('F', 'DISCRETS_12', nk=4, valk=kmess)
            endif
!
            call jevech('PCADISK', 'L', jdc)
            call infdis('REPK', irep, r8bid, k8bid)
            call dcopy(nbt, zr(jdc), 1, klv, 1)
            if (irep .eq. 1) then
                call utpsgl(nno, nc, pgl, zr(jdc), klv)
            endif
            if (zk16(icompo) .eq. 'DIS_CHOC') then
                call r8inir(8, zero, varmo, 1)
                call r8inir(12, zero, dvl, 1)
                call r8inir(12, zero, dpe, 1)
                call r8inir(12, zero, dve, 1)
!              RELATION DE COMPORTEMENT DE CHOC : FORCES NODALES
                call jevech('PVECTUR', 'E', ifono)
                do 501 i = 1, neq
                    zr(ifono+i-1) = 0.d0
                    sim(i) = 0.d0
501              continue
!
                ilogic = 0
                plouf = 0.d0
                call r8inir(3, zero, force, 1)
                call disief(nbt, neq, nno, nc, pgl,&
                            klv, ulp, sim, ilogic, plouf,&
                            sip, fono, force, ndim)
                call dichoc(nbt, neq, nno, nc, zi(lmater),&
                            dul, ulp, zr( igeom), pgl, klv,&
                            duly, dvl, dpe, dve, force,&
                            varmo, varpl, ndim)
                ilogic = 2
                call disief(nbt, neq, nno, nc, pgl,&
                            klv, ulp, sim, ilogic, duly,&
                            sip, zr(ifono), force, ndim)
                do 601 i = 1, neq
                    zr(ifono+i-1) = zr(ifono+i-1)-fono(i)
601              continue
                if (nno .eq. 2) then
                    do 602 i = 1, nc
                        zr(ifono+i-1) = 0.d0
602                  continue
                endif
            endif
        endif
    elseif (option .eq. 'FORC_NODA') then
        call jevech('PCONTMR', 'L', icontg)
        call jevech('PVECTUR', 'E', ivectu)
        if (nno .eq. 1) then
            do in = 1, neq
                fs(in) = zr(icontg+in-1)
            enddo
        else
            do  in = 1, nc
                fs(in) = -zr(icontg+in-1)
                fs(in+nc) = zr(icontg+in+nc-1)
            enddo
        endif
        call jevech('PCAORIE', 'L', lorien)

        call matrot(zr(lorien), pgl)
        if (ndim .eq. 3) then
            call utpvlg(nno, nc, pgl, fs, zr(ivectu))
        else if (ndim.eq.2) then
            call ut2vlg(nno, nc, pgl, fs, zr(ivectu))
        endif
    else
        ASSERT(.false.)
    endif
! --- ------------------------------------------------------------------
    call jedema()
end subroutine
