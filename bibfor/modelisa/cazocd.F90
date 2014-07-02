subroutine cazocd(char, motfac, izone, nzoco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cazouu.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: char
    character(len=16) :: motfac
    integer :: izone, nzoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - LECTURE DONNEES)
!
! LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT (SURFACE IZONE)
!
! ----------------------------------------------------------------------
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR
! IN  IZONE  : INDICE DE ZONE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
!
!
!
    integer :: zcmdf
    character(len=24) :: defico
    integer :: noc, nocn
    character(len=24) :: caradf
    integer :: jcmdf
    character(len=16) :: glis
    real(kind=8) :: aljeu
    real(kind=8) :: coefpt, coefpn, coefff, coefte
    aster_logical :: lcact, lfrot, lpenac, lpenaf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    coefpt = 0.d0
    coefpn = 0.d0
    coefff = 0.d0
    coefte = 0.d0
    aljeu = 0.d0
    lcact = cfdisl(defico,'CONT_ACTI')
    lfrot = cfdisl(defico,'FROTTEMENT')
    lpenaf = cfdisl(defico,'FROT_PENA')
    lpenac = cfdisl(defico,'CONT_PENA')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    caradf = defico(1:16)//'.CARADF'
    zcmdf = cfmmvd('ZCMDF')
    call jeveuo(caradf, 'E', jcmdf)
!
! --- PARAMETRES DU FROTTEMENT
!
    if (lfrot) then
        call getvr8(motfac, 'COULOMB', iocc=izone, scal=coefff, nbret=noc)
        zr(jcmdf+zcmdf*(izone-1)+4-1) = coefff
        call getvr8(motfac, 'COEF_MATR_FROT', iocc=izone, scal=coefte, nbret=noc)
        zr(jcmdf+zcmdf*(izone-1)+1-1) = coefte
    endif
!
! --- CARACTERISTIQUES POUR LES METHODES AVEC PENALISATION
!
    if (lpenac) then
        call getvr8(motfac, 'E_N', iocc=izone, scal=coefpn, nbret=nocn)
        if (nocn .eq. 0) then
            ASSERT(.false.)
        else
            zr(jcmdf+zcmdf*(izone-1)+2-1) = coefpn
        endif
    endif
!
    if (lpenaf) then
        call getvr8(motfac, 'E_T', iocc=izone, scal=coefpt, nbret=nocn)
        if (nocn .eq. 0) then
            ASSERT(.false.)
        else
            zr(jcmdf+zcmdf*(izone-1)+3-1) = coefpt
        endif
    endif
!
! --- OPTION GLISSIERE (POUR CONTRAINTE) : UNIQUE
!
    if (lcact) then
        call cazouu(motfac, nzoco, 'GLISSIERE')
        call getvtx(motfac, 'GLISSIERE', iocc=1, scal=glis, nbret=noc)
        if (glis(1:3) .eq. 'OUI') then
            zr(jcmdf+zcmdf*(izone-1)+6-1) = 1.d0
            call cazouu(motfac, nzoco, 'ALARME_JEU')
            call getvr8(motfac, 'ALARME_JEU', iocc=1, scal=aljeu, nbret=noc)
            zr(jcmdf+zcmdf*(izone-1)+5-1) = aljeu
        else if ((glis(1:3) .eq. 'NON').or.(noc.eq.0)) then
            zr(jcmdf+zcmdf*(izone-1)+6-1) = 0.d0
        else
            ASSERT(.false.)
        endif
    endif
!
    call jedema()
!
end subroutine
