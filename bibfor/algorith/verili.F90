subroutine verili(nomres, ii, fpli1, fpli2, iret)
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
!***********************************************************************
!    P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!  BUT:      < CALCUL DES LIAISONS >
    implicit none
!
!  CALCULER LES NOUVELLES MATRICE DE LIAISON EN TENANT COMPTE
!   DE L'ORIENTATION DES SOUS-STRUCTURES
!  ON DETERMINE LES MATRICE DE LIAISON, LES DIMENSIONS DE CES MATRICES
!  ET LE PRONO ASSOCIE
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT MODE_GENE
! I        /I/: NUMERO INTERFACE COURANTE
! FPLI1    /I/: FAMILLE DES PROFNO DES MATRICES DE LIAISON COTE 1
! FPLI2    /I/: FAMILLE DES PROFNO DES MATRICES DE LIAISON COTE 2
! IRET     /I/: CODE RETOUR DE LA VERIF (=NOMBRE ERREUR, 0=OK)
!
!
!
!
!
!   PARAMETER REPRESENTANT LE NOMBRE MAX DE COMPOSANTE DE LA GRANDEUR
!   SOUS-JACENTE TRAITES
!
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/isgeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
!-----------------------------------------------------------------------
    integer :: i, idim1, idim2, ierd, ii, iret, j
    integer :: lldesc, lllia, llncmp, llpl1, llpl2, nbcmpm, nbec
    integer :: nbecmx, nbnoe1, nbnoe2, numgd
!-----------------------------------------------------------------------
    parameter (nbcmpm =  10)
    parameter (nbecmx =  10)
    character(len=8) :: nomres, nomg, kbid
    character(len=24) :: fpli1, fpli2
    character(len=24) :: valk(6)
    character(len=8) :: sst1, sst2, intf1, intf2, blanc
    integer :: idecp(nbcmpm), idecm(nbcmpm)
    integer :: vali(2)
    integer :: icodp(nbecmx), icodm(nbecmx)
    character(len=1) :: k1bid
!
!-----------------------------------------------------------------------
    data blanc /' '/
!-----------------------------------------------------------------------
!
    call jemarq()
    iret=0
!
!-----RECUPERATION DU NOMBRE DU NOMBRE D'ENTIERS CODES ASSOCIE A DEPL_R
!
    nomg = 'DEPL_R'
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                kbid, ierd)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
!----RECUPERATION DES NOM DE MACR_ELEM ET INTERFACE MIS EN JEU----------
!
    call jeveuo(jexnum(nomres//'      .MODG.LIDF', ii), 'L', lllia)
    sst1=zk8(lllia)
    intf1=zk8(lllia+1)
    sst2=zk8(lllia+2)
    intf2=zk8(lllia+3)
!
!--------------VERIFICATION COHERENCE NOMBRE DE NOEUDS------------------
!
    call jelira(jexnum(fpli1, ii), 'LONMAX', idim1, k1bid)
    nbnoe1=idim1/(1+nbec)
    call jelira(jexnum(fpli2, ii), 'LONMAX', idim2, k1bid)
    nbnoe2=idim2/(1+nbec)
!
    if (nbnoe1 .ne. nbnoe2) then
        valk (1) = sst1
        valk (2) = intf1
        valk (3) = sst2
        valk (4) = intf2
        vali (1) = nbnoe1
        vali (2) = nbnoe2
        call u2mesg('E', 'ALGORITH14_70', 4, valk, 2,&
                    vali, 0, 0.d0)
        iret=1
        goto 9999
    endif
!
!--------RECUPERATION DU NUMERO DE GRANDEUR SOUS-JACENTE----------------
!
    call jeveuo(nomres//'      .MODG.DESC', 'L', lldesc)
    numgd=zi(lldesc+2)
!
!-----------------VERIFICATION SUR LES COMPOSANTES----------------------
!
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', llncmp)
    call jeveuo(jexnum(fpli1, ii), 'L', llpl1)
    call jeveuo(jexnum(fpli2, ii), 'L', llpl2)
!
    do 10 i = 1, nbnoe1
!
        call isgeco(zi(llpl1+(i-1)*(nbec+1)+1), zi(llpl2+(i-1)*(nbec+ 1)+1), nbcmpm, -1, icodp)
        call isgeco(zi(llpl2+(i-1)*(nbec+1)+1), zi(llpl1+(i-1)*(nbec+ 1)+1), nbcmpm, -1, icodm)
!
        call isdeco(icodm, idecm, nbcmpm)
        call isdeco(icodp, idecp, nbcmpm)
!
        do 20 j = 1, nbcmpm
            if (idecp(j) .ne. 0) then
                valk (1) = sst1
                valk (2) = intf1
                valk (3) = zk8(llncmp+j-1)
                valk (4) = sst2
                valk (5) = intf2
                valk (6) = blanc
                call u2mesg('F', 'ALGORITH14_71', 6, valk, 0,&
                            0, 0, 0.d0)
                iret=iret+1
            endif
            if (idecm(j) .ne. 0) then
                valk (1) = sst2
                valk (2) = intf2
                valk (3) = zk8(llncmp+j-1)
                valk (4) = sst1
                valk (5) = intf1
                valk (6) = blanc
                call u2mesg('F', 'ALGORITH14_72', 6, valk, 0,&
                            0, 0, 0.d0)
                iret=iret+1
            endif
20      continue
10  end do
!
!
9999  continue
    call jedema()
end subroutine
