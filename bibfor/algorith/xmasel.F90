subroutine xmasel(nnop, ddlh, nfe, ddlc, igeom,&
                  imate, pintt, cnset, heavt, lonch,&
                  basloc, lsn, lst, matuu)
    implicit none
    include 'jeveux.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/elref5.h'
    include 'asterfort/xmase2.h'
    include 'asterfort/xmase3.h'
    integer :: nnop, imate, igeom
    integer :: ddlh, nfe, ddlc, cnset(4*32), heavt(36), lonch(10)
    real(kind=8) :: pintt(3*11), lsn(nnop)
    real(kind=8) :: lst(nnop), matuu(*), basloc(*)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!
!     BUT:  PRÉLIMINAIRES AU CALCUL DES OPTIONS RIGI_MECA_TANG,
!           RAPH_MECA ET FULL_MECA  EN HYPER-ELASTICITE AVEC X-FEM
!
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  IPOIDS  : POIDS DES POINTS DE GAUSS
! IN  IVF     : VALEUR  DES FONCTIONS DE FORME
! IN  DDLH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  IGEOM   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  NOMTE   : NOM DU TE
! IN  IMATE   : MATERIAU CODE
! IN  LGPG  : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!              CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  PINTT   : COORDONNÉES DES POINTS D'INTERSECTION
! IN  CNSET   : CONNECTIVITE DES SOUS-ELEMENTS
! IN  HEAVT   : VALEURS DE L'HEAVISIDE SUR LES SS-ELTS
! IN  LONCH   : LONGUEURS DES CHAMPS UTILISÉES
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! OUT MATUU   : MATRICE DE MASSE PROFIL
!
! ......................................................................
!
!
!
!
    integer :: nse, npg, ndim
    integer :: j, ise, in, ino
    integer :: ibid
!
    real(kind=8) :: he, coorse(12)
!
    character(len=8) :: elrefp, elrese(3), fami(3)
!
    data    elrese /'SE2','TR3','TE4'/
    data    fami   /'BID','XINT','XINT'/
!
! ----------------------------------------------------------------------
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
    call elref1(elrefp)
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM
    call elref4(' ', 'RIGI', ndim, ibid, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NPG
    call elref5(elrese(ndim), fami(ndim), ibid, ibid, ibid,&
                npg, ibid, ibid, ibid, ibid,&
                ibid, ibid)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=lonch(1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES 4/3 SOMMETS DU SOUS-TETRA/TRIA
        do 111 in = 1, ndim+1
            ino=cnset((ndim+1)*(ise-1)+in)
            do 112 j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else
                    coorse(ndim*(in-1)+j)=pintt(ndim*(ino-1000-1)+j)
                endif
112          continue
111      continue
!
!       FONCTION HEAVYSIDE CSTE SUR LE SS-ELT
        he = heavt(ise)
!
!       DEBUT DE LA ZONE MEMOIRE DE SIG ET VI CORRESPONDANTE
!
        if (ndim .eq. 3) then
!
            call xmase3(elrefp, ndim, coorse, igeom, he,&
                        ddlh, ddlc, nfe, basloc, nnop,&
                        npg, imate, lsn, lst, matuu)
!
        else if (ndim.eq.2) then
!
            call xmase2(elrefp, ndim, coorse, igeom, he,&
                        ddlh, ddlc, nfe, basloc, nnop,&
                        npg, imate, lsn, lst, matuu)
!
        endif
!
!
110  end do
!
end subroutine
