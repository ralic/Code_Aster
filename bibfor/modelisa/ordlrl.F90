subroutine ordlrl(charge, lisrel, nomgd)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8gaem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/ordrel.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
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
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=19), intent(in) :: lisrel
    character(len=8), intent(in) :: charge
    character(len=8), intent(in) :: nomgd
!
! -----------------------------------------------------------------
!     MISE A JOUR DE L'OBJET DE TYPE  LISTE_RELA ET DE NOM
!     LISREL  :
!               LES RELATIONS SONT REORDONNEES  PAR ORDRE DE NOEUD
!               CROISSANT ET POUR UN NOEUD DONNE PAR DDL CROISSANT
!
!               LES RELATIONS STRICTEMENT EGALES SONT ELIMINEES
!               (I.E. ON NE GARDE QUE LA RELATION DE PLUS GRAND
!                     INDICE DANS LISREL)
! -----------------------------------------------------------------
!     L'OBJET LISREL DOIT OBLIGATOIREMENT EXISTER
! -----------------------------------------------------------------
!  CHARGE        - IN/JXIN    - K8  - : NOM DE LA SD_CHARGE
!  LISREL        - IN/JXVAR   - K19  - : NOM DE LA LISTE_RELA
! -------------------------------------------------------
!
!
! --------- VARIABLES LOCALES ---------------------------
    character(len=24) :: valk(2)
    integer :: nmocl
    parameter(nmocl=300)
    complex(kind=8) :: coproc, rapcoc
    character(len=4) :: typcoe
    character(len=8) :: nomnoe
    character(len=8) :: noma, mod, cmp, nomcmp(nmocl)
    character(len=16) :: kidrel
    character(len=19) :: ligrmo
! --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
    real(kind=8) :: copror, difrel, eps1, eps2, epsrel, rapcoe, coemax
    integer :: i, icmp, icomp,  iddl, iddl1, iddl2, ideca1, ideca2
    integer :: idecal, in, indmax, ino
    integer ::  inom,  ipntr1, ipntr2, ipntrl, irela, irela1
    integer :: irela2
    integer ::  jprnm, jrlco, jrlco1, jrlco2,  jrlcof
    integer :: jrldd
    integer :: jrlno, idnoe1, idnoe2, idnoeu
    integer :: nbcmp, nbec, nbrela, nbtema, nbter1, nbter2, nbterm
    integer :: nddla, nidrel
    integer, pointer :: rlnt(:) => null()
    character(len=8), pointer :: rltc(:) => null()
    integer, pointer :: rlsu(:) => null()
    integer, pointer :: rlpo(:) => null()
    integer, pointer :: rlnr(:) => null()
    character(len=8), pointer :: lgrf(:) => null()
    complex(kind=8), pointer :: coef_c(:) => null()
    integer, pointer :: coefmax(:) => null()
    real(kind=8), pointer :: coef_r(:) => null()
    integer, pointer :: noeud_occ(:) => null()
    integer, pointer :: noeud_rela(:) => null()
!
    call jemarq()
!
    eps1=1.d2*r8prem()
    eps2=1.d0/r8gaem()
!
! - Mesh and model
!
    call dismoi('NOM_MODELE', charge, 'CHARGE', repk=mod)
    ligrmo=mod(1:8)//'.MODELE'
    call jeveuo(ligrmo//'.LGRF', 'L', vk8=lgrf)
    noma=lgrf(1)
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nbcmp)
    nddla=nbcmp-1
    ASSERT(nddla.le.nmocl)
!
    do i = 1, nbcmp
        nomcmp(i)=zk8(inom-1+i)
    end do
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
! --- ACCES AUX COMPOSANTES DE LA LISTE_RELA
!
    call jeveuo(lisrel//'.RLCO', 'E', jrlco)
    call jeveuo(lisrel//'.RLDD', 'E', jrldd)
    call jeveuo(lisrel//'.RLNO', 'E', jrlno)
    call jeveuo(lisrel//'.RLNT', 'E', vi=rlnt)
    call jeveuo(lisrel//'.RLPO', 'E', vi=rlpo)
    call jeveuo(lisrel//'.RLSU', 'E', vi=rlsu)
    call jeveuo(lisrel//'.RLTC', 'L', vk8=rltc)
!
! --- TYPE DE VALEUR DES COEFFICIENTS DES RELATIONS ---
!
    typcoe=rltc(1)(1:4)
!
! --- NOMBRE DE RELATIONS DE LA LISTE_RELA
!
    call jeveuo(lisrel//'.RLNR', 'L', vi=rlnr)
    nbrela=rlnr(1)
!
! --- NOMBRE DE TERMES  MAX IMPLIQUES DANS UNE RELATION
!
    nbtema=0
    do irela = 1, nbrela
        if (nbtema .lt. rlnt(irela)) nbtema=rlnt(irela)
    end do
!
! --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
! --- L'INDICE DU PLUS GRAND COEFFICIENT EN VALEUR ABSOLUE
! --- (MODULE) D'UNE RELATION
!
    AS_ALLOCATE(vi=coefmax, size=nbrela)
!
! --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
! --- LES NUMEROS DES NOEUDS D'UNE RELATION
!
    AS_ALLOCATE(vi=noeud_rela, size=nbtema)
!
! --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
! --- LE NOMBRE D'OCCURENCES DE CHAQUE NOEUD APPRAISSANT
! --- DANS UNE RELATION
!
    AS_ALLOCATE(vi=noeud_occ, size=nbtema)
!
! --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
! --- LES COEFFICIENTS REELS D'UNE RELATION
!
    AS_ALLOCATE(vr=coef_r, size=nbtema)
!
! --- CREATION D'UN VECTEUR DE TRAVAIL DESTINE A CONTENIR
! --- LES COEFFICIENTS COMPLEXES D'UNE RELATION
!
    AS_ALLOCATE(vc=coef_c, size=nbtema)
!
!
!
!     1. ON ORDONNE LES TERMES DE CHAQUE RELATION POUR POUVOIR
!        LES COMPARER PLUS FACILEMENT ET DETECTER LES DOUBLONS
!     ----------------------------------------------------------
    do irela = 1, nbrela
        ipntrl=rlpo(irela)
        nbterm=rlnt(irela)
        idecal=ipntrl-nbterm
        jrlcof=jrlco+idecal
        idnoeu=jrlno+idecal
        iddl=jrldd+idecal
!
        if (typcoe .eq. 'COMP') then
            do ino = 1, nbterm
                coef_c(ino)=zc(jrlcof+ino-1)
            enddo
        else if (typcoe .eq. 'REEL') then
            do ino = 1, nbterm
                coef_r(ino)=zr(jrlcof+ino-1)
            enddo
        else
            ASSERT(.false.)
        endif
!
        do ino = 1, nbterm
            nomnoe=zk8(idnoeu+ino-1)
            call jenonu(jexnom(noma//'.NOMNOE', nomnoe), in)
            noeud_rela(ino)=in
            cmp=zk8(iddl+ino-1)
            icmp=indik8(nomcmp,cmp,1,nbcmp)
            if (.not.exisdg(zi(jprnm-1+(in-1)*nbec+1),icmp)) then
                valk(1)=cmp
                valk(2)=nomnoe
                call utmess('F', 'CHARGES2_31', nk=2, valk=valk)
            endif
        enddo
!
! ----- Rearrangement of linear relation tables in ascending order of nodes and dof for given node
!
        call ordrel(noeud_rela, zk8(idnoeu), zk8(iddl), coef_r, coef_c,&
                    noeud_occ, nbterm, zk8(inom), nddla)
!
!       -- REAFFECTATION DU TABLEAU DES COEFFICIENTS
        if (typcoe .eq. 'COMP') then
            do ino = 1, nbterm
                zc(jrlcof+ino-1)=coef_c(ino)
            enddo
        else if (typcoe .eq. 'REEL') then
            do ino = 1, nbterm
                zr(jrlcof+ino-1)=coef_r(ino)
            enddo
        else
            ASSERT(.false.)
        endif
!
        coemax=0.0d0
        if (typcoe .eq. 'COMP') then
            do ino = 1, nbterm
                if (abs(coef_c(ino)) .gt. coemax) then
                    coemax=abs(coef_c(ino))
                    indmax=ino
                endif
            enddo
        else if (typcoe .eq. 'REEL') then
            do ino = 1, nbterm
                if (abs(coef_r(ino)) .gt. coemax) then
                    coemax=abs(coef_r(ino))
                    indmax=ino
                endif
            enddo
        else
            ASSERT(.false.)
        endif
        coefmax(irela)=indmax
    end do
!
!
!
!   2. IDENTIFICATION DES RELATIONS REDONDANTES A 1 TERME
!   ----------------------------------------------------------------
    call jecreo('&&ORDLRL.KIDREL', 'V N K16')
    call jeecra('&&ORDLRL.KIDREL', 'NOMMAX', nbrela)
    do irela1 = nbrela, 1, -1
        nbter1=rlnt(irela1)
        if (nbter1 .le. 1) then
            ipntr1=rlpo(irela1)
            ideca1=ipntr1-nbter1
            idnoe1=jrlno+ideca1
            iddl1=jrldd+ideca1
            kidrel=zk8(idnoe1)//zk8(iddl1)
            call jenonu(jexnom('&&ORDLRL.KIDREL', kidrel), nidrel)
            if (nidrel .eq. 0) then
                call jecroc(jexnom('&&ORDLRL.KIDREL', kidrel))
            else
                rlsu(irela1)=1
            endif
        endif
    end do
    call jedetr('&&ORDLRL.KIDREL')
!
!
!
!   3. IDENTIFICATION DES RELATIONS REDONDANTES A PLUSIEURS TERMES
!   ----------------------------------------------------------------
    do irela1 = nbrela, 2, -1
        nbter1=rlnt(irela1)
        if (nbter1 .eq. 1) goto 170
        ipntr1=rlpo(irela1)
        ideca1=ipntr1-nbter1
        jrlco1=jrlco+ideca1
        idnoe1=jrlno+ideca1
        iddl1=jrldd+ideca1
!
        indmax=coefmax(irela1)
!
        if (typcoe .eq. 'COMP') then
            if (abs(zc(jrlco1+indmax-1)) .lt. eps2) then
                call utmess('F', 'CHARGES2_32')
            endif
        else if (typcoe .eq. 'REEL') then
            if (abs(zr(jrlco1+indmax-1)) .lt. eps2) then
                call utmess('F', 'CHARGES2_32')
            endif
        else
            ASSERT(.false.)
        endif
!
!
!       --  CAS DES COEF. COMPLEXES
!       -----------------------------------
        if (typcoe .eq. 'COMP') then
            do irela2 = 1, irela1-1
                nbter2=rlnt(irela2)
                ipntr2=rlpo(irela2)
                ideca2=ipntr2-nbter2
                jrlco2=jrlco+ideca2
                idnoe2=jrlno+ideca2
                iddl2=jrldd+ideca2
                coproc=zc(jrlco2+indmax-1)/zc(jrlco1+indmax-1)
!
                if (nbter1 .eq. nbter2) then
                    icomp=0
                    do ino = 1, nbter1
                        if (zk8(idnoe1+ino-1) .eq. zk8(idnoe2+ino-1)) then
                            if (zk8(iddl1+ino-1) .eq. zk8(iddl2+ino-1)) then
                                rapcoc=coproc*zc(jrlco1+ino-1)
                                epsrel=eps1*abs(zc(jrlco1+ino-1))
                                difrel=abs(zc(jrlco2+ino-1)-rapcoc)
                                if (difrel .le. epsrel) goto 110
                                icomp=1
                                goto 120
                            else
                                icomp=1
                                goto 120
                            endif
                        else
                            icomp=1
                            goto 120
                        endif
110                     continue
                    enddo
120                 continue
                    if (icomp .eq. 0) rlsu(irela2)=1
                endif
            enddo
!
!
!       --  CAS DES COEF. REEL
!       -----------------------------------
        else if (typcoe .eq. 'REEL') then
            do irela2 = 1, irela1-1
                nbter2=rlnt(irela2)
                ipntr2=rlpo(irela2)
                ideca2=ipntr2-nbter2
                jrlco2=jrlco+ideca2
                idnoe2=jrlno+ideca2
                iddl2=jrldd+ideca2
                copror=zr(jrlco2+indmax-1)/zr(jrlco1+indmax-1)
!
                if (nbter1 .eq. nbter2) then
                    icomp=0
                    do ino = 1, nbter1
                        if (zk8(idnoe1+ino-1) .eq. zk8(idnoe2+ino-1)) then
                            if (zk8(iddl1+ino-1) .eq. zk8(iddl2+ino-1)) then
                                rapcoe=copror*zr(jrlco1+ino-1)
                                epsrel=eps1*abs(zr(jrlco1+ino-1))
                                difrel=abs(zr(jrlco2+ino-1)-rapcoe)
                                if (difrel .le. epsrel) goto 140
                                icomp=1
                                goto 150
                            else
                                icomp=1
                                goto 150
                            endif
                        else
                            icomp=1
                            goto 150
                        endif
140                     continue
                    enddo
150                 continue
                    if (icomp .eq. 0) rlsu(irela2)=1
                endif
            enddo
        else
            ASSERT(.false.)
        endif
170     continue
    end do
!
!
! ---  MENAGE  ---
    AS_DEALLOCATE(vi=noeud_rela)
    AS_DEALLOCATE(vi=noeud_occ)
    AS_DEALLOCATE(vi=coefmax)
    AS_DEALLOCATE(vr=coef_r)
    AS_DEALLOCATE(vc=coef_c)
!
    call jedema()
end subroutine
