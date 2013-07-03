subroutine palino(nomaz, mcfact, mcgrno, mcno, iocc,&
                  noml)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomaz, mcfact, mcgrno, mcno, noml
    integer :: iocc
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
!
! BUT : LECTURE DE LA LISTE DE NOEUDS DECRITS PAR LA SEQUENCE :
!       MCFAC : ( MCGRNO : LISTE DE GROUP_NO ,
!                 MCNO   : LISTE DE NOEUD   , ....)
!       CREATION D'UN OBJET DE NOM : NOML  OJB V V I DIM=NBNO+1
!       NBNO : NOMBRE DE NOEUDS LUS
!       NOML(1) = NBNO  NOML(1+I)=INO NUM.DUNOEUD DANS NOMAZ
!
! IN   NOMAZ   : NOM DU MAILLAGE
! IN   MCFACT K*(*) : MOT CLE FACTEUR
! IN   MCGRNO K*(*) : MOT CLE CONSERNANT LA LISTE DE GROUP_NO
! IN   MCNO   K*(*) : MOT CLE CONSERNANT LA LISTE DE NOEUDS
! IN   IOCC   I     : NUMERO DE L'OCCURENCE DU MOT CLE FACTEUR
! OUT  NOML   K*24  : NOM DE L'OBJET JEVEUX CREE SUR LA VOLATILE
!
    character(len=8) :: noma
    character(len=16) :: mcf, tymocl(2), limocl(2)
    character(len=24) :: liste1
    integer :: j1, j2, n1, k
!
    call jemarq()
    noma = nomaz
    mcf = mcfact
    tymocl(1)='GROUP_NO'
    limocl(1)= mcgrno
    tymocl(2)='NOEUD'
    limocl(2)= mcno
!
    liste1='&&PALINO.LISTE'
!
    call reliem(' ', noma, 'NU_NOEUD', mcf, iocc,&
                2, limocl, tymocl, liste1, n1)
    call jedetr(noml)
    call wkvect(noml, 'V V I', n1+1, j2)
    zi(j2)=n1
    if (n1 .gt. 0) then
        call jeveuo(liste1, 'L', j1)
        do 1, k=1,n1
        zi(j2+k)=zi(j1-1+k)
 1      continue
    endif
!
!
    call jedetr(liste1)
    call jedema()
end subroutine
