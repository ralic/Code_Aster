subroutine exlima(motfaz, iocc, base, modelz, ligrel)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/getvtx.h"
#include "asterfort/gnoms2.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
    character(len=*) :: motfaz, base, modelz, ligrel
    integer :: iocc
!     -----------------------------------------------------------------
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
! but  :  scruter les mots cle tout/group_ma/maille pour creer
!         un ligrel "reduit" a partir du ligrel du modele modelz
!
! in  : modelz : nom du modele
!
! out/jxout   : ligrel  : ligrel reduit
!     attention :
!          - le nom de ligrel est toujours "out"
!          - parfois on rend ligrel=ligrel(modele) :
!             - alors on ne tient donc pas compte de 'base'
!             - il ne faut pas le detruire !
!          - parfois on en cree un nouveau sur la base 'base'
!             - le nom du ligrel est obtenu par gnomsd
!  -----------------------------------------------------------------
!
    integer :: n1, jma, nbma
    character(len=8) :: modele, noma
    character(len=16) :: motfac, motcle(2), typmcl(2), oper, k16b
    character(len=19) :: ligrmo
    character(len=24) :: lismai, noojb
!  -----------------------------------------------------------------
!
    motfac = motfaz
    modele = modelz
    if (modele .eq. ' ') then
        call utmess('F', 'UTILITAI8_10')
    endif
!
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    lismai = '&&EXLIMA.LISTE_MAILLES'
!
!
!     --  SI ON DOIT TOUT PRENDRE , LIGREL = LIGRMO
!     ------------------------------------------------------
    if (motfac .ne. ' ') then
        if (getexm(motfac,'TOUT') .eq. 1) then
            call getvtx(motfac, 'TOUT', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .ne. 0) goto 9998
        endif
    else
        call getvtx(' ', 'TOUT', nbval=0, nbret=n1)
        if (n1 .ne. 0) goto 9998
    endif
!
!
!
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- CREATION ET AFFECTATION DU VECTEUR DE K8 DE NOM LISMAI
!     CONTENANT LES NOMS DES MAILLES FORMANT LE LIGREL A CREER
!     --------------------------------------------------------
    call reliem(modele, noma, 'NU_MAILLE', motfac, iocc,&
                2, motcle(1), typmcl(1), lismai, nbma)
!
!     -- SI LES MOTS CLES GROUP_MA ET MAILLE N'ONT PAS ETE UTILISES:
    if (nbma .eq. 0) goto 9998
!
!
!
! --- CREATION DU LIGREL
!     ---------------------------------
    call getres(k16b, k16b, oper)
    if (oper .ne. 'IMPR_RESU') then
        noojb='12345678.LIGR000000.LIEL'
        call gnomsd(' ', noojb, 14, 19)
    else
!     -- DANS LE CAS IMPR_RESU, GNOMSD NE PEUT PAS SERVIR CAR
!        LA COMMANDE NE CREE PAS DE CONCEPT
        ASSERT(base.eq.'V')
        noojb='&&EXLIMA.LIGR000000.LIEL'
        call gnoms2(noojb, 14, 19)
    endif
    ligrel=noojb(1:19)
    ASSERT(ligrel(1:8).ne.' ')
    call jeveuo(lismai, 'L', jma)
    call exlim1(zi(jma), nbma, modele, base, ligrel)
    call jedetr(lismai)
    goto 999
!
!
9998 continue
    ligrel = ligrmo
!
999 continue
!
!
end subroutine
