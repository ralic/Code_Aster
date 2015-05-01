subroutine palima(nomaz, mcfact, mcgrma, mcma, iocc,&
                  noml)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomaz, mcfact, mcgrma, mcma, noml
    integer :: iocc
!-----------------------------------------------------------------------
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
!
! BUT : LECTURE DE LA LISTE DES MAILLES DESCRITES PAR LA SEQUENCE :
!       MCFAC : ( MCGRMA : LISTE DE GROUP_MA ,
!                 MCMA   : LISTE DE MAILLE   , ....)
!       CREATION D'UN OBJET DE NOM : NOML  OJB V V I DIM=NBMA*2+1
!       NBMA : NOMBRE DE MAILLES LUES
!       NOML(1) = NBMA  NOML(1+2*(I-1)+1)=IMA NUM.DE LA MAILLE DANS NOMA
!                       NOML(1+2*(I-1)+2)=ITYP NUM.DU TYPE_MAIL DE IMA
!       LES MAILLES SONT TRIEES EN PAQUETS DE MEME TYPEMAIL
!
! IN   NOMAZ   : NOM DU MAILLAGE
! IN   MCFACT K* : MOT CLE FACTEUR
! IN   MCGRMA K* : MOT CLE CONSERNANT LA LISTE DE GROUP_MA
! IN   MCMA   K* : MOT CLE CONSERNANT LA LISTE DE MAILLE
! IN   IOCC   I     : NUMERO DE L'OCCURENCE DU MOT CLE FACTEUR
! OUT  NOML   K*24  : NOM DE L'OBJET JEVEUX CREE SUR LA VOLATILE
!
    character(len=8) :: noma
    character(len=16) :: mcf, tymocl(2), limocl(2)
    character(len=24) :: liste1
    integer ::  k, n1, j1, j2, ima
    integer, pointer :: typmail(:) => null()
!
    call jemarq()
    noma = nomaz
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    mcf = mcfact
    tymocl(1)='GROUP_MA'
    limocl(1)= mcgrma
    tymocl(2)='MAILLE'
    limocl(2)= mcma
!
    liste1='&&PALIMA.LISTE'
!
    call reliem(' ', noma, 'NU_MAILLE', mcf, iocc,&
                2, limocl, tymocl, liste1, n1)
    call jedetr(noml)
    call wkvect(noml, 'V V I', 2*n1+1, j2)
    zi(j2)=n1
    if (n1 .gt. 0) then
        call jeveuo(liste1, 'L', j1)
        do 1, k=1,n1
        ima=zi(j1-1+k)
        zi(j2+2*(k-1)+1)=ima
        zi(j2+2*(k-1)+2)=typmail(ima)
 1      continue
    endif
!
!
    call jedetr(liste1)
    call jedema()
end subroutine
