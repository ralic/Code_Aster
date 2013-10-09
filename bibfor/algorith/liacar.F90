subroutine liacar(nomres, sst, intf, fplin, fplio,&
                  ii, icar)
    implicit none
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
! P. RICHARD     DATE 13/10/92
!-----------------------------------------------------------------------
!    BUT : CALCUL DES CARACTERISTIQUES DES MATRICES DE LIAISON ORIENTEES
!
!          CALCULER LE NOMBRE DE LIGNES, DE COLONNES, ET LE PROFIL
!          LE PROFIL DE LA MATRICE DE LIAISON NON ORIENTEE EST EGALEMENT
!          CALCULE
!          POUR L'INSTANT ON EST A UN BLOC
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM UTILISATEUR DU RESULTAT MODELE_GENE
! SST    /I/ : NOM DE LA SOUS-STRUCTURE
! INTF   /I/ : NOM DE L'INTERFACE
! FPLIO  /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON ORIENTEES
! FPLIN  /I/ : FAMILLE DES PROFNO MATRICES DE LIAISON NON ORIENTEES
! II     /I/ : NUMERO INTERFACE COURANTE
! ICAR   /O/ : VECTEUR DES CARACTERISTIQUES LIAISON
!
!
!
#include "jeveux.h"
#include "asterfort/bmnoin.h"
#include "asterfort/dismoi.h"
#include "asterfort/exprli.h"
#include "asterfort/intet0.h"
#include "asterfort/iscode.h"
#include "asterfort/isdeco.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mgutdm.h"
#include "asterfort/pmppr.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
!
!
!
!   PARAMETER REPRESENTANT LE NOMBRE MAX DE COMPOSANTES DE LA GRANDEUR
!   SOUS-JACENTE TRAITEE
!
    integer :: nbcmpm, ldlid, ordo, ntail, nbec, ibid, llrot, ldprof, k1
    integer :: k2, i, j, kk, l, k, nbnoe, ii, ldprli, icomp, nblig, nbcol
    parameter    (nbcmpm=10)
    integer :: ideco(nbcmpm), idecn(nbcmpm), icar(3), idecw(nbcmpm)
    integer :: idecw2(nbcmpm)
    character(len=8) :: nomres, sst, intf, kbid, nommcl, basmod, nomg
    character(len=24) :: fplio, fplin, famli
    real(kind=8) :: rot(3), matrot(nbcmpm, nbcmpm), zero, epsi
    real(kind=8) :: matbuf(nbcmpm, nbcmpm), mattmp(nbcmpm, nbcmpm)
    real(kind=8) :: codn(nbcmpm, nbcmpm), codw
!
!-----------------------------------------------------------------------
    data zero /0.0d+00/
    data epsi /1.0d-10/
!-----------------------------------------------------------------------
!
    call jemarq()
    ntail=nbcmpm**2
!
!-----RECUPERATION DU NOMBRE DU NOMBRE D'ENTIERS CODES ASSOCIE A DEPL_R
!
    nomg = 'DEPL_R'
    call dismoi('NB_EC', nomg, 'GRANDEUR', repi=nbec)
    if (nbec .gt. 10) then
        call utmess('F', 'MODELISA_94')
    endif
!
! --- RECUPERATION DES NOMS DU MACR_ELEM ET DE LA BASE MODALE DE SST
!
    call mgutdm(nomres, sst, ibid, 'NOM_MACR_ELEM', ibid,&
                nommcl)
    call mgutdm(nomres, sst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
!
! --- RECUPERATION DES ROTATIONS DE LA SOUS-STRUCTURE
!
    call jenonu(jexnom(nomres//'      .MODG.SSNO', sst), ibid)
    call jeveuo(jexnum(nomres//'      .MODG.SSOR', ibid), 'L', llrot)
    do i = 1, 3
        rot(i)=zr(llrot+i-1)
    end do
!
! --- CALCUL DE LA MATRICE DE ROTATION
!
    call intet0(rot(1), mattmp, 3)
    call intet0(rot(2), matrot, 2)
    call r8inir(ntail, zero, matbuf, 1)
    call pmppr(mattmp, nbcmpm, nbcmpm, 1, matrot,&
               nbcmpm, nbcmpm, 1, matbuf, nbcmpm,&
               nbcmpm)
    call intet0(rot(3), mattmp, 1)
    call pmppr(matbuf, nbcmpm, nbcmpm, 1, mattmp,&
               nbcmpm, nbcmpm, 1, matrot, nbcmpm,&
               nbcmpm)
!
! --- RECUPERATION DES DONNEES MINI-PROFNO DE LA LIAISON
!
    call mgutdm(nomres, sst, ibid, 'NOM_BASE_MODALE', ibid,&
                basmod)
    kbid=' '
    call bmnoin(basmod, kbid, intf, ibid, 0,&
                [0], nbnoe)
!
    kbid=' '
!
    famli=nomres//'      .MODG.LIDF'
    call jeveuo(jexnum(famli, ii), 'L', ldlid)
    if ((zk8(ldlid+2).eq.sst) .and. (zk8(ldlid+4).eq.'OUI     ')) then
        ordo=1
    else
        ordo=0
    endif
!
    call exprli(basmod, kbid, intf, ibid, fplin,&
                ii, ordo)
    call jeveuo(jexnum(fplin, ii), 'L', ldprli)
!
    call jecroc(jexnum(fplio, ii))
    call jeecra(jexnum(fplio, ii), 'LONMAX', nbnoe*(1+nbec))
    call jeveuo(jexnum(fplio, ii), 'E', ldprof)
!
! INITIALISATION
    do k1 = 1, nbcmpm
        do k2 = 1, nbcmpm
            codn(k1,k2)=zero
        end do
    end do
!
! --- BOUCLE SUR LES NOEUDS DU MINI-PROFNO POUR REMPLISSAGE ET COMPTAGE
!
    icomp=0
    do i = 1, nbnoe
        call isdeco(zi(ldprli+(i-1)*(1+nbec)+1), ideco, nbcmpm)
        do j = 1, nbcmpm
! ICI ON FABRIQUE UN VECTEUR D ENTIERS DE CMP 1 SUR LA
! CMP COURANTE 0 AILLEURS
            do kk = 1, nbcmpm
                if (kk .eq. j) then
                    idecw(kk)=1
                else
                    idecw(kk)=0
                endif
            end do
! ON MULTIPLIE LE VECTEUR D'ENTIERS TRADUISANT LA PRESENCE DES
! CMPS SUR LE NOEUD PAR LE VECTEUR DE TRAVAIL EFFECTUE
! PRECEDEMMENT DE TELLE SORTE QU ON NE FAIT ICI QUE LA ROTATION
! DE LA CMP COURANTE -IE NUMERO J
! LE CRITERE DE PRESENCE DE LA CMP DANS L INTERFACE ORIENTE
! DEVIENT DONC VALABLE- IE IDECN
            do l = 1, nbcmpm
                idecw2(l)=ideco(l)*idecw(l)
            end do
            do k1 = 1, nbcmpm
                do k2 = 1, nbcmpm
                    codn(j,k1)=codn(j,k1)+matrot(k1,k2)*idecw2(k2)
                end do
            end do
        end do
!
        do k = 1, nbcmpm
            codw=zero
            do j = 1, nbcmpm
                codw=codw+abs(codn(j,k))
            end do
!
            if (codw .gt. epsi) then
                idecn(k)=1
            else
                idecn(k)=0
            endif
        end do
        call iscode(idecn, zi(ldprof+(i-1)*(nbec+1)+1), nbcmpm)
        zi(ldprof+(i-1)*(nbec+1))=icomp+1
        do k = 1, nbcmpm
            icomp=icomp+idecn(k)
        end do
    end do
    nblig=icomp
!
    icar(1)=nblig
    call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbcol)
    icar(2)=nbcol
!
    call jedema()
end subroutine
