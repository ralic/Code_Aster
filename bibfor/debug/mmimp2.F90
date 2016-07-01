subroutine mmimp2(ifm, noma, ligrcf, jtabf)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
#include "jeveux.h"
!
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
    integer :: ifm
    character(len=19) :: ligrcf
    character(len=8) :: noma
    integer :: jtabf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE - IMPRESSIONS)
!
! AFFICHAGE POUR LES ELEMENTS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  LIGRCF : LIGREL POUR LES ELEMENTS DE CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! IN  JTABF  : POINTEUR VERS DEFICO(1:16)//'.CARACF'
!
!
!
!
    integer :: ztabf
    integer ::   ilcnx1
    integer :: nbnoe, nbnom, nummae, nummam, itymae, itymam, ityctc
    character(len=8) ::  ntymae, ntymam, ntyctc, nommae, nommam
    character(len=8) :: nomnoe, nomnom
    integer :: numnoe, numnom, inoe, inom
    integer :: nbel, nndel, jad, iel
    integer, pointer :: connex(:) => null()
    integer, pointer :: typmail(:) => null()
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    ztabf = cfmmvd('ZTABF')
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
! --- NOMBRE D'ELEMENTS DE CONTACT
!
    call jelira(ligrcf//'.NEMA', 'NUTIOC', nbel)
    write(ifm,1000) nbel
!
    do 20 iel = 1, nbel
!
!       -- ACCES A L'ELEMENT DE CONTACT EN COURS
        call jeveuo(jexnum(ligrcf//'.NEMA', iel), 'L', jad)
!
!       -- NOMBRE DE NOEUDS TOTAL DE L'ELEMENT DE CONTACT
        call jelira(jexnum(ligrcf//'.NEMA', iel), 'LONMAX', nndel)
!
!       -- TYPE DE L'ELEMENT DE CONTACT
        ityctc = zi(jad+nndel-1)
        call jenuno(jexnum('&CATA.TE.NOMTE', ityctc), ntyctc)
!
!       -- IMPRESSION POUR ELEMENT DE CONTACT
        write(ifm,1050) iel,ntyctc,nndel-1
!
!       -- INFOS SUR MAILLE ESCLAVE
        nummae = nint(zr(jtabf+ztabf*(iel-1)+1))
        call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
        itymae = typmail(nummae)
        call jenuno(jexnum('&CATA.TM.NOMTM', itymae), ntymae)
        nbnoe = zi(ilcnx1+nummae) - zi(ilcnx1-1+nummae)
!
!       -- IMPRESSION POUR MAILLE ESCLAVE
        write(ifm,1060) nommae,ntymae,nbnoe
!
        do 21 inoe = 1, nbnoe
            numnoe = connex(1+zi(ilcnx1-1+nummae)-2+inoe)
            call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
            write (ifm,1001) nomnoe
21      continue
        1001  format (' <CONTACT>        NOEUD :',a8)
!
!       -- INFOS SUR MAILLE MAITRE
        nummam = nint(zr(jtabf+ztabf*(iel-1)+2))
        call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
        itymam = typmail(nummam)
        call jenuno(jexnum('&CATA.TM.NOMTM', itymam), ntymam)
        nbnom = zi(ilcnx1+nummam) - zi(ilcnx1-1+nummam)
!
!       -- IMPRESSION POUR MAILLE MAITRE
        write(ifm,1070) nommam,ntymam,nbnom
        do 31 inom = 1, nbnom
            numnom = connex(1+zi(ilcnx1-1+nummam)-2+inom)
            call jenuno(jexnum(noma//'.NOMNOE', numnom), nomnom)
            write (ifm,1001) nomnom
31      continue
!       -- IMPRESSION POUR NOEUD DE CONTACT
        write(ifm,1075) iel
!
20  end do
!
! --- FORMATS AFFICHAGE
!
    1000 format (' <CONTACT> CREATION DES ',i5,' ELEMENTS DE CONTACT')
    1050 format (' <CONTACT>     * L''ELEMENT DE CONTACT ',i5,&
     &        ' EST DE TYPE ',a8,&
     &        ' AVEC ',i5,' NOEUDS')
!
    1060 format (' <CONTACT>     ** EST CREE ENTRE LA MAILLE ESCLAVE ',a8,&
     &        ' DE TYPE ',a8,&
     &        ' AVEC ',i5,' NOEUDS')
!
    1070 format (' <CONTACT>     **             ET LA MAILLE MAITRE  ',a8,&
     &        ' DE TYPE ',a8,&
     &        ' AVEC ',i5,' NOEUDS')
    1075 format (' <CONTACT>     **             POUR LE POINT DE CONTACT',&
     &        i5)
!
    call jedema()
!
end subroutine
