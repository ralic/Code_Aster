subroutine uttcpr(nommes, nbv, temps)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
    character(len=*) :: nommes
    integer :: nbv
    real(kind=8) :: temps(nbv)
! ----------------------------------------------------------------------
! ROUTINE DE MESURE DU TEMPS CPU.
! BUT : RECUPERER LES VALEURS D'UNE MESURE DE TEMPS
!
! IN  NOMMES    : NOM IDENTIFIANT LA MESURE
!
! IN  NBV       : DIMENSION DU TABLEAU TEMPS
! OUT TEMPS     : TABLEAU CONTENANT LES MESURES
! ----------------------------------------------------------------------
!    TEMPS(1) TEMPS CPU RESTANT EN SECONDES
!    TEMPS(2) NOMBRE D'APPEL A DEBUT/FIN
!    TEMPS(3) TEMPS CPU TOTAL
!    TEMPS(4) TEMPS CPU MOYEN
!    TEMPS(5) TEMPS CPU USER TOTAL
!    TEMPS(6) TEMPS CPU SYSTEME
!    TEMPS(7) TEMPS ELAPSED
!
! RQUE : LES VALEURS STOCKEES SONT ACCUMUEES VIA UTTCPU
! ----------------------------------------------------------------------
    logical :: ljev
    integer :: indi, jvalms, k
!
!
!     -- COMMONS POUR MESURE DE TEMPS :
    integer :: mtpniv, mtpsta, indmax
    parameter (indmax=5)
    character(len=80) :: snolon(indmax)
    real(kind=8) :: valmes(indmax*7), valmei(indmax*7)
    common /mestp1/ mtpniv,mtpsta
    common /mestp2/ snolon
    common /mestp3/ valmes,valmei
! ----------------------------------------------------------------------
!
!     1. CALCUL DE INDI ET LJEV :
!     -------------------------------
!     -- POUR CERTAINES MESURES, ON NE PEUT PAS FAIRE DE JEVEUX :
!        ON GARDE ALORS LES INFOS DANS LES COMMON MESTPX
    if (nommes .eq. 'CPU.MEMD.1') then
        indi=1
    else if (nommes.eq.'CPU.MEMD.2') then
        indi=2
    else
        ljev=.true.
        call jenonu(jexnom('&&UTTCPU.NOMMES', nommes), indi)
        if (indi .eq. 0) goto 9999
        goto 9998
    endif
    call assert(indi.le.indmax)
    ljev=.false.
!
!
!     2. RECUPERATION DES TEMPS :
!     -------------------------------
9998  continue
    if (ljev) then
        call jeveuo('&&UTTCPU.VALMES', 'L', jvalms)
        do 1, k=1,nbv
        temps(k)= zr(jvalms-1+7*(indi-1)+k)
 1      continue
    else
        do 2, k=1,nbv
        temps(k)= valmes(7*(indi-1)+k)
 2      continue
    endif
!
!
9999  continue
end subroutine
