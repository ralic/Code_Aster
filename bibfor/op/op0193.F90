subroutine op0193()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR  PROJ_MESU_MODAL
!
!     EXTRAPOLATION DE RESULTATS DE MESURES EXPERIMENTALES SUR UN MODELE
!     NUMERIQUE EN DYNAMIQUE
!     ------------------------------------------------------------------
!
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mpmod2.h"
#include "asterfort/mptran.h"
    integer :: n1, nbmesu, nbmode
!
    character(len=8) :: basemo, nommes
    character(len=24) :: vrange, vnoeud, basepr, vcham
!
!DEB
!
    call jemarq()
    call infmaj()
!
! --- RECUPERATION DE LA BASE DE PROJECTION ---
!
    call getvid('MODELE_CALCUL', 'BASE', iocc=1, scal=basemo, nbret=n1)
!
! --- RECUPERATION DE LA MESURE
!
    call getvid('MODELE_MESURE', 'MESURE', iocc=1, scal=nommes, nbret=n1)
!
! --- PROJECTION SUR LE MODELE NUMERIQUE
!
    call mpmod2(basemo, nommes, nbmesu, nbmode, basepr,&
                vnoeud, vrange, vcham)
!
! --- ECRITURE SD RESULTAT (TRAN_GENE, HARM_GENE OU MODE_GENE)
!
    call mptran(basemo, nommes, nbmesu, nbmode, basepr,&
                vnoeud, vrange, vcham)
!
    call jedema()
!
end subroutine
