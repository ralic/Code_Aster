subroutine cmqlma(main, maout, nbma, mailq)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/jeccta.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: nbma, mailq(nbma)
    character(len=8) :: main, maout
!
!-----------------------------------------------------------------------
!    - COMMANDE :  CREA_MAILLAGE / QUAD_LINE
!    - BUT DE LA COMMANDE:
!      TRANSFORMATION DES MAILLES QUADRATIQUES -> LINEAIRES
!    - BUT DE LA ROUTINE:
!      CREATION DES OBJETS .TYPMAIL .CONNEX DE LA NOUVELLE SD MAILLAGE
!    - ROUTINE APPELEE PAR : CMQLQL
! ----------------------------------------------------------------------
! IN        MAIN    K8   NOM DU MAILLAGE INITIAL
! IN        MAOUT   K8   NOM DU MAILLAGE FINAL
! IN        NBMA     I   NOMBRE DE MAILLES REFERENCEES
! IN        MAILS    I   NUMERO DES MAILLES REFERENCEES
! ----------------------------------------------------------------------
!
!  ======           ==============         ========================
!  MAILLE           TYPE APRES LIN         NBRE DE NOEUDS APRES LIN
!  ======           ==============         ========================
!
!
!  SEG3                      2                       2
!  TRIA6                     7                       3
!  QUAD8                    12                       4
!  QUAD9                    12                       4
!  TETRA10                  18                       4
!  PENTA15                  20                       6
!  PENTA18                  20                       6
!  PYRAM13                  23                       5
!  HEXA20                   25                       8
!  HEXA27                   25                       8
!
!
    integer :: nbtyma
    parameter(nbtyma=27)
    integer :: jdim, i, nbtma, jma, jtypm1, jtypm2, jconn1, jconn2
    integer :: tymal(nbtyma), num1, num2, nbnol(nbtyma), ityp, j, inom, nbnomx
    integer :: iret, ndim, ij
    character(len=1) :: kbid
    character(len=8) :: nomnoi
    character(len=24) :: connex, typma
!
!     TYMAL: TYPE DES MAILLES APRES LINEARISATION (CF. CI-DESSUS)
!     NBNOL: NOMBRE DE NOEUDS APRES LINEARISATION (CF. CI-DESSUS)
    data tymal   /3*0,2,4*0,7,4*0,12,0,12,2*0,18,0,20,20,0,23,0,25,25/
    data nbnol   /3*0,2,4*0,3,4*0,4 ,0,4 ,2*0,4 ,0,6 ,6, 0,5 ,0,8, 8/
!
    call jemarq()
!
    connex = maout // '.CONNEX'
    typma= maout // '.TYPMAIL'
!
!     CREATION D'UN TABLEAU DIMENSIONNE AU NOMBRE DE MAILLES DU
!     MAILLAGE INITIAL PERMETTANT DE SAVOIR SI LA MAILLE EN COURS
!     SERA LINEAIRE OU NON.
!     -----------------------------------------------------------
    call jeveuo(main//'.DIME', 'L', jdim)
    nbtma=zi(jdim+2)
    call wkvect('&&CMQLMA.MAILLE', 'V V I', nbtma, jma)
    do 10 i = 1, nbtma
        zi(jma+i-1)=0
10  end do
    do 20 i = 1, nbma
        zi(jma+mailq(i)-1)=1
20  end do
!
!     CREATION DES OBJETS  '.TYPMAIL', '.CONNEX':
!     -------------------------------------------
!
    call dismoi('F', 'NB_NO_MAX', '&CATA', 'CATALOGUE', nbnomx,&
                kbid, iret)
    call jecrec(maout//'.CONNEX', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                zi(jdim+2))
    ndim=nbnomx*zi(jdim+2)
    call jeecra(maout//'.CONNEX', 'LONT', ndim, kbid)
    call jeveuo(main//'.TYPMAIL', 'L', jtypm1)
    call jeveuo(typma, 'E', jtypm2)
    call jeveuo('&CATA.TM.NBNO', 'L', inom)
!
!     NUM1:NOMBRE DE NOEUDS DE LA MAILLE INITIALE
!     NUM2:NOMBRE DE NOEUDS DE LA MAILLE LINEARISEE
!
!     ON PARCOURT LES MAILLES DU MAILLAGE
    do 30 i = 1, nbtma
        num1=zi(inom+zi(jtypm1+i-1)-1)
!
!        '.TYPMAIL':
!        ----------
!        SI LA MAILLE N'EST PAS A LINEARISER
        if (zi(jma+i-1) .eq. 0) then
            zi(jtypm2+i-1)=zi(jtypm1+i-1)
            num2=num1
!        SI LA MAILLE EST A LINEARISER
        else
            ityp=zi(jtypm1+i-1)
            zi(jtypm2+i-1)=tymal(ityp)
            num2=nbnol(ityp)
        endif
!
!        '.CONNEX':
!        ----------
        call jecroc(jexnum(connex, i))
        call jeecra(jexnum(connex, i), 'LONMAX', num2, kbid)
        call jeveuo(jexnum(connex, i), 'E', jconn2)
        call jeveuo(jexnum(main//'.CONNEX', i), 'L', jconn1)
        do 40 j = 1, num2
            ij=zi(jconn1+j-1)
            call jenuno(jexnum(main//'.NOMNOE', ij), nomnoi)
            call jenonu(jexnom(maout//'.NOMNOE', nomnoi), zi(jconn2+j-1))
40      continue
30  end do
!
!     -- RETASSAGE  DE CONNEX (QUI A ETE ALLOUEE TROP GRANDE) :
    call jeccta(connex)
!
    call jedetr('&&CMQLMA.MAILLE')
!
    call jedema()
!
end subroutine
