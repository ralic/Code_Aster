subroutine nmadev(sddisc, sderro, iterat)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit     none
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmltev.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc
    character(len=24) :: sderro
    integer :: iterat
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DE L'INDICATEUR DE SUCCES DES ITERATIONS DE NEWTON
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! IN  ITERAT : NUMERO DE L'ITERATION DE NEWTON
!
! ----------------------------------------------------------------------
!
    integer :: ibid, vali, nbok
    integer :: iadapt, nadapt
    real(kind=8) :: r8bid, vale
    character(len=8) :: k8bid, cricom, metlis
    character(len=16) :: nopara
    character(len=19) :: even
    logical(kind=1) :: itemax, lerrit, divres, cvnewt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LA MISE A JOUR DE L'INDICATEUR DE SUCCES DES ITERATIONS DE NEWTON
! --- N'EST FAITE QU'EN GESTION AUTO DU PAS DE TEMPS
!
    call utdidt('L', sddisc, 'LIST', ibid, 'METHODE',&
                r8bid, ibid, metlis)
    if (metlis .ne. 'AUTO') goto 9999
!
! --- EVENEMENTS
!
    call nmerge(sderro, 'ITER_MAXI', itemax)
    call nmltev(sderro, 'ERRI', 'NEWT', lerrit)
    call nmerge(sderro, 'DIVE_RESI', divres)
!
! --- NEWTON A CONVERGE ?
!
    call nmlecv(sderro, 'NEWT', cvnewt)
!
! --- BOUCLE SUR LES OCCURENCES D'ADAPTATION
!
    call utdidt('L', sddisc, 'LIST', ibid, 'NADAPT',&
                r8bid, nadapt, k8bid)
!
    do 10 iadapt = 1, nadapt
!
! ----- NOM DE L'EVENEMENT
!
        call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_EVEN',&
                    r8bid, ibid, even)
!
        if (even .eq. 'SEUIL_SANS_FORMULE') then
!
! ------- PARAMETRES DU SEUIL
!
            call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_PARA',&
                        r8bid, ibid, nopara)
            call utdidt('L', sddisc, 'ADAP', iadapt, 'CRIT_COMP',&
                        r8bid, ibid, cricom)
            call utdidt('L', sddisc, 'ADAP', iadapt, 'VALE',&
                        vale, vali, k8bid)
!
            ASSERT(nopara.eq.'NB_ITER_NEWT')
!
! ------- RECUP DU NB DE SUCCES CONSECUTIFS : NBOK
!
            call utdidt('L', sddisc, 'ADAP', iadapt, 'NB_EVEN_OK',&
                        r8bid, nbok, k8bid)
!
! ------- EN CAS DE NOUVEAU SUCCES A CONVERGENCE
!
            if (cvnewt) then
                if (cricom .eq. 'LT' .and. iterat .lt. vali .or. cricom .eq. 'GT' .and.&
                    iterat .gt. vali .or. cricom .eq. 'LE' .and. iterat .le. vali .or.&
                    cricom .eq. 'GE' .and. iterat .ge. vali) then
                    nbok = nbok+1
                endif
            endif
!
! ------- EN CAS D'ECHEC: ON REMET A ZERO
!
            if (lerrit .or. itemax .or. divres) nbok=0
!
! ------- ENREGISTREMENT DU NB DE SUCCES CONSECUTIFS
!
            call utdidt('E', sddisc, 'ADAP', iadapt, 'NB_EVEN_OK',&
                        r8bid, nbok, k8bid)
!
        endif
10  end do
!
9999  continue
!
    call jedema()
end subroutine
