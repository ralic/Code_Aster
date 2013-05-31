subroutine pacoje(coniz, iocc, motfaz, nomaz, conrz,&
                  ndim)
! aslint: disable=
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/cacono.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/normev.h'
    include 'asterfort/palima.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: coniz, motfaz, nomaz, conrz
    character(len=8) :: noma
    character(len=16) :: motfac
    character(len=24) :: coni, conr
    integer :: iocc, ndim
! ---------------------------------------------------------------------
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
!     BUT : CREATION ET AFFECTATION DE LA .S.D. CONR
!           CONTENANT LES COMPOSANTES DU VECTEUR NORMAL
!           POUR CHAQUE NOEUD EN VIS-A-VIS DONNE PAR
!           LA S.D. CONI, AINSI QUE LE JEU ENTRE CES NOEUDS.
!           LA LISTE DES MAILLES AUXQUELLES APPARTIENNENT CES NOEUDS
!           EST RELUE DANS CETTE ROUTINE.
!
!           CONR EST A L'INSTAR DE CONI UNE COLLECTION
!           ET C'EST L'OBJET D'INDICE IOCC DE CETTE COLLECTION
!           QUI EST CREE ET AFFECTE.
!
!
!
! IN  CONIZ  K24  : S.D. /
!                   CONI(1) = NBCOUPLE (NOMBRE DE COUPLES DE NOEUDS
!                                       EN VIS-A-VIS)
!                   I = 1, NBCOUPLE
!                       CONI(2*(I-1)+1) = NUNO1
!                       CONI(2*(I-1)+2) = NUNO2
!
!                   NUNO1 ET NUNO2 SONT LES 2 NUMEROS DE NOEUDS
!                   EN VIS-A-VIS.
!
! IN  IOCC     I  : SI >0 ON TRAITE L'OCCURENCE IOCC DE MOTFAC
!                         (CAS DE LIAISON_GROUP)
!                   SI <0 ERREUR FATALE
!
! IN  MOTFAZ  K16  : MOT CLE FACTEUR A TRAITER = LIAISON_GROUP
!
! IN  NOMAZ    K8  : NOM DU MAILLAGE
!
! OUT CONRZ    K24 : S.D. /
!                     CONRZ EST CREE ET AFFECTE DANS LA ROUTINE
!                     LE NOM CONRZ EST UNE ENTREE DE LA ROUTINE
!                     CONRZ CONTIENT LES VALEURS DES COMPOSANTES
!                     NORMALES AUX NOEUDS EN VIS-A-VIS ET LE
!                     JEU ENTRE CES NOEUDS.
!
!                    CONR : OJB BASE V R DIM = 12 * NBCOUPLE EN 2D
!                                              22 * NBCOUPLE EN 3D
!                      I = 1, NBCOUPLE ,  J = 1, 3
!                      CONR( (2*NDIM+1)*(I-1)+J      )  = NORM1(J)
!                      CONR( (2*NDIM+1)*(I-1)+J+NDIM )  = NORM2(J)
!
!                      CONR( (2*NDIM+1)*I            )  = JEU
!
! ---------------------------------------------------------------------
!
!
!
    integer :: nbcmp, ivale, idconi, nbcoup, idrad, idangl, no1, no2, i, j
    integer :: inoma
    real(kind=8) :: norm1(3), norm2(3), jeu
    real(kind=8) :: nrmbid, nor1(3), nor2(3)
    character(len=24) :: llist1, llist2
!     -----------------------------------------------------------------
!
    call jemarq()
    coni = coniz
    conr = conrz
    noma = nomaz
    motfac = motfaz
!
    nbcmp = 3
    call jeveuo(noma(1:8)//'.COORDO    .VALE', 'L', ivale)
!
    if (motfac .eq. 'LIAISON_GROUP') then
        call jeveuo(jexnum(coni, iocc), 'L', idconi)
        nbcoup = zi(idconi)
!
        call jecroc(jexnum(conr, iocc))
!
        if (ndim .eq. 2) then
            call jeecra(jexnum(conr, iocc), 'LONMAX', 12*nbcoup, ' ')
        else
            call jeecra(jexnum(conr, iocc), 'LONMAX', 22*nbcoup, ' ')
        endif
        call jeveuo(jexnum(conr, iocc), 'E', idrad)
    else
        call assert(.false.)
    endif
!
    call wkvect('&&PACOJE.ANGL', 'V V R', nbcoup, idangl)
!
!
    do 10 i = 1, nbcoup
!
        no1 = zi(idconi+2*(i-1)+1)
        no2 = zi(idconi+2*(i-1)+2)
!
        do 20 j = 1, 3
            norm1(j) = 0.0d0
            norm2(j) = 0.0d0
20      continue
!
! ----- CONSTRUCTION DE LA LISTE DE MAILLES LLIST1 SPECIFIEE APRES
! ----- LES MOTS-CLES GROUP_MA_1 OU MAILLE_1
!
        llist1 = '&&PACOJE.LLIST1'
        call palima(noma, motfac, 'GROUP_MA_1', 'MAILLE_1', iocc,&
                    llist1)
!
! ----- CONSTRUCTION DE LA LISTE DE MAILLES LLIST2 SPECIFIEE APRES
! ----- LES MOTS-CLES GROUP_MA_2 OU MAILLE_2
!
        llist2 = '&&PACOJE.LLIST2'
        call palima(noma, motfac, 'GROUP_MA_2', 'MAILLE_2', iocc,&
                    llist2)
!
! ----- DETERMINATION DU VECTEUR NORMAL NOR1 (RESP. NOR2)
! ----- AUX MAILLES DE LA LISTE LLIST1 (RESP. LLIST2)
! ----- AUXQUELLES APPARTIENT LE NOEUD NO1 (RESP. NO2),
! ----- CALCULE AU NOEUD NO1 (RESP. NO2)
!
        call cacono(noma, ndim, llist1, llist2, no1,&
                    no2, nor1, nor2, inoma)
!
        do 40 j = 1, ndim
            norm1(j) = norm1(j) + nor1(j)
            norm2(j) = norm2(j) + nor2(j)
40      continue
!
!       SI INOMA = -1 => NORM1 = 0 CAR MAILLE POI1
!       SI INOMA = -2 => NORM2 = 0 CAR MAILLE POI1
!
        if (inoma .ne. -1) then
            call normev(norm1, nrmbid)
        endif
        if (inoma .ne. -2) then
            call normev(norm2, nrmbid)
        endif
        jeu = 0.0d0
!
!       ANGLE ENTRE NORMALES ET MOYENNE DES NORMALES UNITILE SI POI1
!
        if ((inoma.ne.-1) .and. (inoma.ne.-2)) then
!
            do 50 j = 1, ndim
                norm1(j) = (norm1(j) - norm2(j))/2.0d0
50          continue
!
        else if (inoma.eq.-1) then
!
!          NO1 APPARTIENT A UNE MAILLE POI1 : LA NORMALE EST NORM2
!
            do 51 j = 1, ndim
                norm1(j) = - norm2(j)
51          continue
!
        else if (inoma.eq.-2) then
!
!          NO2 APPARTIENT A UNE MAILLE POI1 : LA NORMALE EST NORM1
!
        endif
!
!
        do 70 j = 1, ndim
            zr(idrad-1+(2*ndim+1)*(i-1)+j) = norm1(j)
            zr(idrad-1+(2*ndim+1)*(i-1)+j+ndim) = norm2(j)
!
            jeu = jeu - zr(&
                  ivale-1+nbcmp*(no1-1)+j) * norm1(j) + zr(ivale-1+nbcmp*(no2-1)+j) * norm1(j)
70      continue
!
        zr(idrad- 1+(2*ndim+1)*i) = jeu
!
!
10  end do
!
    call jedetr(llist1)
    call jedetr(llist2)
    call jedetr('&&PACOJE.ANGL')
! FIN -----------------------------------------------------------------
    call jedema()
end subroutine
