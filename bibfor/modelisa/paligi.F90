subroutine paligi(pheno, modl, ligrch, igrel, inema,&
                  iliste)
    implicit none
#include "jeveux.h"
!
#include "asterfort/ini002.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
    character(len=4) :: pheno
    character(len=*) :: modl, ligrch
    integer :: igrel, inema, iliste(*)
!---------------------------------------------------------------------
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
!     PAROIS DEFINI PAR LA LISTE ILISTE DESCRIVANT LES VIS A VIS ENTRE
!     DES MAILLES(UN OBJET DE LA COLLECTION GENEREE PAR PATRMA)
! ARGUMENTS D'ENTREE:
! IN   PHENO  K4  : PHENOMENE ( 'THER' )
! IN   MODL   K*  : MODELISATION
! IN   LIGRCH K19 : NOM DU LIGREL DE CHARGE
! IN   IGREL  I   : NUMERO DU GREL DE CHARGE
! VAR  INEMA  I   : NUMERO  DE LA DERNIERE MAILLE TARDIVE DANS LIGRCH
! IN   ILISTE I(*): TABLEAU DESCRIVANT LES VIS A VIS
!                  (1) = ITYPM : NUM. DU TYPE_MAIL
!                  (2) = NBMA  : NBRE DE VIS A VIS
!                  (3) = NBTOT :NBRE DE NOEUDS DES MAILLES EN VIS A VIS
!          POUR IC = 1,NBMA
!    V(3+(IC-1)*(2+2*NBNTOT)+1)= NUMA1 NUM. DE LA 1ERE MAILLE DU COUPLE
!    V(3+(IC-1)*(2+2*NBNTOT)+2)= NUMA2 NUM. DE LA 2EME MAILLE DU COUPLE
!                        EN VIS A VIS AVEC NUMA1
!           POUR INO = 1,NBNTOT
! V(3+(IC-1)*(2+2*NBTOT)+2+2*(INO-1)+1)=N1(INO)NUM.DU NO. INO DE NUMA1
! V(3+(IC-1)*(2+2*NBTOT)+2+2*(INO-1)+1)= N2(N1(INO)) NUM.DU NOEUD DE
!                   NUMA2 EN VIS A VIS AVEC LE NOEUD N1(INO)
    character(len=16) :: typelm
    character(len=19) :: ligr
    character(len=24) :: liel, nema
    integer :: nmaxob
!-----------------------------------------------------------------------
    integer :: idliel, idnema, ima, ino, itypel, jma, nbma
    integer :: nbno
!-----------------------------------------------------------------------
    parameter (nmaxob=30)
    integer :: itabl(nmaxob), nval
    character(len=24) :: k24tab(nmaxob)
!
! --- DEBUT
    call jemarq()
    nbma = iliste(2)
    nbno = iliste(3)
    ligr = ligrch
    liel = ligr//'.LIEL'
    nema = ligr//'.NEMA'
!
! --- TYPE_ELEM ASSOCIE AUX MAILLES DE COUPLAGE
    typelm = pheno(1:4)
    if (modl(1:2) .eq. 'PL') then
        typelm(3:6) = 'PLSE'
        if (nbno .eq. 2) then
            typelm(7:8) = '22'
        else if (nbno.eq.3) then
            typelm(7:8) = '33'
        else
            call u2mess('F', 'MODELISA6_9')
        endif
    else if (modl(1:2).eq.'AX') then
        typelm(3:6) = 'PLSE'
        if (nbno .eq. 2) then
            typelm(7:8) = '22'
        else if (nbno.eq.3) then
            typelm(7:8) = '33'
        else
            call u2mess('F', 'MODELISA6_9')
        endif
    else if (modl(1:2).eq.'3D') then
        typelm(5:9) = '_FACE'
        if (nbno .eq. 3) then
            typelm(10:11) = '33'
        else if (nbno.eq.4) then
            typelm(10:11) = '44'
        else if (nbno.eq.6) then
            typelm(10:11) = '66'
        else if (nbno.eq.8) then
            typelm(10:11) = '88'
        else if (nbno.eq.9) then
            typelm(10:11) = '99'
        else
            call u2mess('F', 'MODELISA6_9')
        endif
        call ini002(typelm, nmaxob, itabl, k24tab, nval)
    else
        call u2mess('F', 'MODELISA6_14')
    endif
    call jenonu(jexnom('&CATA.TE.NOMTE', typelm), itypel)
!
! --- ON CREE ET ON REMPLI LE GREL
!
    call jecroc(jexnum(liel, igrel))
    call jeecra(jexnum(liel, igrel), 'LONMAX', nbma+1)
    call jeveuo(jexnum(liel, igrel), 'E', idliel)
    do 20 ima = 1, nbma
        inema = inema + 1
        jma = 3 + (ima-1)*2* (nbno+1)
        zi(idliel-1+ima) = -inema
        call jecroc(jexnum(nema, inema))
        call jeecra(jexnum(nema, inema), 'LONMAX', 2*nbno+1)
        call jeveuo(jexnum(nema, inema), 'E', idnema)
        do 10 ino = 1, nbno
            zi(idnema-1+ino) = iliste(jma+2+2*ino-1)
            zi(idnema-1+nbno+ino) = iliste(jma+2+2*ino)
10      continue
        zi(idnema+2*nbno) = iliste(1)
20  end do
    zi(idliel+nbma) = itypel
    call jedema()
end subroutine
