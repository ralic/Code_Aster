subroutine irgmsh(nomcon, partie, ifi, nbcham, cham,&
                  lresu, nbordr, ordr, nbcmp, nomcmp,&
                  nbmat, nummai, versio, lgmsh, tycha)
    implicit none
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/irgmce.h"
#include "asterfort/irgmcg.h"
#include "asterfort/irgmcn.h"
#include "asterfort/irgmma.h"
#include "asterfort/irgmpf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ifi, nbcham, nbordr, nbcmp, ordr(*), nbmat, nummai(*), versio
    logical :: lresu, lgmsh
    character(len=*) :: nomcon
    character(len=*) :: cham(*), nomcmp(*), partie
    character(len=8) :: tycha
!     ------------------------------------------------------------------
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
!     BUT: ECRITURE D'UN CHAMP OU D'UN CONCEPT RESULTAT AU FORMAT GMSH
!
!     ENTREE:
!     NOMCON : K8  : NOM DU CONCEPT A IMPRIMER
!     PARTIE : K4  : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
!     IFI    : I   : NUMERO D'UNITE LOGIQUE DU FICHIER GMSH
!     NBCHAM : I   : NOMBRE DE CHAMP DANS LE TABLEAU CHAM
!     CHAM   : K16 : NOM DES CHAMPS SYMBOLIQUES A IMPRIMER (EX 'DEPL',
!     LRESU  : L   : INDIQUE SI NOMCON EST UN CHAMP OU UN RESULTAT
!     NBORDR : I   : NOMBRE DE NUMEROS D'ORDRE DANS LE TABLEAU ORDR
!     ORDR   : I   : LISTE DES NUMEROS D'ORDRE A IMPRIMER
!     NBCMP  : I   : NOMBRE DE COMPOSANTES A IMPRIMER
!     NOMCMP : K*  : NOMS DES COMPOSANTES A IMPRIMER
!     NBMAT  : I   : NOMBRE DE MAILLES A IMPRIMER
!     NUMMAI : I   : NUMEROS DES MAILLES A IMPRIMER
!     VERSIO : I   : =1 SI TOUTES LES MAILLES SONT DES TRIA3 OU DES TET4
!                    =2 SINON ( LES MAILLLES SONT LINEAIRES)
!     TYCHA  : K8  : TYPE DE CHAMP A IMPRIMER (VERSION >= 1.2)
!                    = SCALAIRE/VECT_2D/VECT_3D/TENS_2D/TENS_3D
!
!     ------------------------------------------------------------------
    integer :: ior, ich, iret, nbma, i
    integer :: typpoi, typseg, typtri, typtet, typqua, typpyr, typpri, typhex
    integer :: jcoor, jconx, jpoin, jpara, iad
    character(len=8) :: tych, noma, k8b, nomaou, valk(2)
    character(len=16) :: valk2(2)
    character(len=19) :: noch19, noco19
!
!     --- TABLEAU DE DECOUPAGE
    integer :: ntyele
    parameter (ntyele = 28)
!     NBRE, NOM D'OBJET POUR CHAQUE TYPE D'ELEMENT
    integer :: nbel(ntyele)
    character(len=24) :: nobj(ntyele)
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DU MAILLAGE, NB_MAILLE, ...
!
    if (lresu) then
        do ior = 1, nbordr
            do ich = 1, nbcham
                call rsexch(' ', nomcon, cham(ich), ordr(ior), noch19,&
                            iret)
                if (iret .eq. 0) goto 34
            end do
        end do
        call utmess('A', 'PREPOST2_59')
        goto 999
 34     continue
    else
        noch19 = nomcon
    endif
    call dismoi('NOM_MAILLA', noch19, 'CHAMP', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
! --- ECRITURE DE L'ENTETE DU FICHIER AU FORMAT GMSH
!
    if (.not.lgmsh) then
        call irgmpf(ifi, versio)
        lgmsh = .true.
    endif
!
! --- RECUPERATION DES INSTANTS, FREQUENCES, ...
!
    call wkvect('&&IRGMSH.PARA', 'V V R', max(1, nbordr), jpara)
    if (lresu) then
        noco19=nomcon
!
!        -- DANS UN EVOL_NOLI, IL PEUT EXISTER INST ET FREQ.
!           ON PREFERE INST :
        call jenonu(jexnom(noco19//'.NOVA', 'INST'), iret)
        if (iret .ne. 0) then
            do ior = 1, nbordr
                call rsadpa(nomcon, 'L', 1, 'INST', ordr(ior),&
                            0, sjv=iad, styp=k8b)
                zr(jpara+ior-1) = zr(iad)
            end do
        else
            call jenonu(jexnom(noco19//'.NOVA', 'FREQ'), iret)
            if (iret .ne. 0) then
                do ior = 1, nbordr
                    call rsadpa(nomcon, 'L', 1, 'FREQ', ordr(ior),&
                                0, sjv=iad, styp=k8b)
                    zr(jpara+ior-1) = zr(iad)
                end do
            endif
        endif
    else
        zr(jpara) = 0.d0
    endif
!
! --- TRANSFORMATION DU MAILLAGE EN MAILLAGE SUPPORTE PAR GMSH
!
! --- INIT
    do i = 1, ntyele
        nbel(i) = 0
        nobj(i) = ' '
    end do
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1' ), typpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2' ), typseg)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3' ), typtri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4' ), typqua)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4' ), typtet)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5' ), typpyr)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6' ), typpri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8' ), typhex)
    nobj(typpoi) = '&&IRGMSH_POI'
    nobj(typseg) = '&&IRGMSH_SEG'
    nobj(typtri) = '&&IRGMSH_TRI'
    nobj(typqua) = '&&IRGMSH_QUA'
    nobj(typtet) = '&&IRGMSH_TET'
    nobj(typpyr) = '&&IRGMSH_PYR'
    nobj(typpri) = '&&IRGMSH_PRI'
    nobj(typhex) = '&&IRGMSH_HEX'
!
    nomaou = '&&MAILLA'
    call irgmma(noma, nomaou, nbmat, nummai, 'V',&
                nobj, nbel, versio)
!
    call jeveuo(nomaou//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(nomaou//'.CONNEX', 'L', jconx)
    call jeveuo(jexatr(nomaou//'.CONNEX', 'LONCUM'), 'L', jpoin)
!
!
! --- BOUCLE SUR LE NOMBRE DE CHAMPS A IMPRIMER
!
    do ich = 1, nbcham
!
        if (lresu) then
!        --VERIFICATION DE L'EXISTENCE DU CHAMP CHAM(ICH) DANS LA
!          SD RESULTAT NOMCON POUR LE NO. D'ORDRE ORDR(1)
!          ET RECUPERATION DANS NOCH19 DU NOM SI LE CHAM_GD EXISTE
            call rsexch(' ', nomcon, cham(ich), ordr(1), noch19,&
                        iret)
            if (iret .ne. 0) goto 10
        else
            noch19 = nomcon
        endif
!
! ------ RECHERCHE DU TYPE DU CHAMP (CHAM_NO OU CHAM_ELEM)
!
        call dismoi('TYPE_CHAMP', noch19, 'CHAMP', repk=tych)
!
! ------ TRAITEMENT DU CAS CHAM_NO:
!
        if (tych(1:4) .eq. 'NOEU') then
            call irgmcn(cham(ich), partie, ifi, nomcon, ordr,&
                        nbordr, zr(jcoor), zi(jconx), zi(jpoin), nobj,&
                        nbel, nbcmp, nomcmp, lresu, zr(jpara),&
                        versio, tycha)
!
! ------ TRAITEMENT DU CAS CHAM_ELEM AUX NOEUDS:
!
        else if (tych(1:4).eq.'ELNO') then
            if (tycha(1:4) .eq. 'VECT') then
                valk(1)=tycha
                valk(2)=tych(1:4)
                call utmess('A', 'PREPOST6_35', nk=2, valk=valk)
                tycha='SCALAIRE'
            endif
            call irgmce(cham(ich), partie, ifi, nomcon, ordr,&
                        nbordr, zr(jcoor), zi(jconx), zi(jpoin), nobj,&
                        nbel, nbcmp, nomcmp, lresu, zr(jpara),&
                        nomaou, noma, versio, tycha)
!
!
! ------ TRAITEMENT DU CAS CHAM_ELEM AUX GAUSS:
!
        else if (tych(1:4).eq.'ELGA' .or. tych(1:4).eq.'ELEM') then
            if (tycha(1:4) .eq. 'VECT' .or. tycha(1:4) .eq. 'TENS') then
                valk(1)=tycha
                valk(2)=tych(1:4)
                call utmess('A', 'PREPOST6_35', nk=2, valk=valk)
            endif
            call irgmcg(cham(ich), partie, ifi, nomcon, ordr,&
                        nbordr, zr(jcoor), zi(jconx), zi(jpoin), nobj,&
                        nbel, nbcmp, nomcmp, lresu, zr(jpara),&
                        nomaou, versio)
!
! ------ AUTRE: PAS D'IMPRESSION
!
        else
            valk2(1) = cham(ich)
            valk2(2) = tych
            call utmess('I', 'PREPOST2_60', nk=2, valk=valk2)
        endif
 10     continue
    end do
!
! --- MENAGE
!
    do i = 1, ntyele
        if (nobj(i) .ne. ' ') then
            call jedetr(nobj(i))
        endif
    end do
!
    call detrsd('MAILLAGE', nomaou)
    call jedetr(nomaou//'.NUMOLD')
    call jedetr('&&IRGMSH.PARA')
!
999 continue
!
    call jedema()
!
end subroutine
