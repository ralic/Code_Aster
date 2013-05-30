subroutine irmmno(idfimd, nomamd, ndim, nbnoeu, coordo,&
                  nomnoe)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     ECRITURE DU MAILLAGE -  FORMAT MED - LES NOEUDS
!        -  -     -                  -         --
!-----------------------------------------------------------------------
!     ENTREE:
!       IDFIMD  : IDENTIFIANT DU FICHIER MED
!       NOMAMD : NOM DU MAILLAGE MED
!       NDIM   : DIMENSION DU PROBLEME (2  OU 3)
!       NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!       COORDO : VECTEUR DES COORDONNEES DES NOEUDS
!       NOMNOE : VECTEUR NOMS DES NOEUDS
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mfcooe.h'
    include 'asterfort/mfnome.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    integer :: idfimd
    integer :: ndim, nbnoeu
!
    real(kind=8) :: coordo(*)
!
    character(len=*) :: nomnoe(*)
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMMNO' )
!
    integer :: edfuin
    parameter (edfuin=0)
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: tygeno
    parameter (tygeno=0)
!
    integer :: codret
    integer :: iaux, ino
    integer :: jcoord
    integer :: ifm, nivinf
    integer :: adnomn
!
    character(len=8) :: saux08
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    call infniv(ifm, nivinf)
!
!====
! 2. ECRITURE DES COORDONNEES DES NOEUDS
!    LA DIMENSION DU PROBLEME PHYSIQUE EST VARIABLE (1,2,3), MAIS
!    ASTER STOCKE TOUJOURS 3 COORDONNEES PAR NOEUDS.
!====
!
! 2.1. ==> ECRITURE
! 2.1.1. ==> EN DIMENSION 3, ON PASSE LE TABLEAU DES COORDONNEES
!
!    LE TABLEAU COORDO EST UTILISE AINSI : COORDO(NDIM,NBNOEU)
!    EN FORTRAN, CELA CORRESPOND AU STOCKAGE MEMOIRE SUIVANT :
!    COORDO(1,1), COORDO(2,1), COORDO(3,1), COORDO(1,2), COORDO(2,2),
!    COORDO(3,2), COORDO(1,3), ... , COORDO(1,NBNOEU), COORDO(2,NBNOEU),
!    COORDO(3,NBNOEU)
!    C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!
    if (ndim .eq. 3) then
!
        call mfcooe(idfimd, nomamd, coordo, edfuin, nbnoeu,&
                    codret)
!
    else
!
! 2.1.2. ==> AUTRES DIMENSIONS : ON CREE UN TABLEAU COMPACT DANS LEQUEL
!            ON STOCKE LES COORDONNEES, NOEUD APRES NOEUD.
!            C'EST CE QUE MED APPELLE LE MODE ENTRELACE.
!
        call wkvect('&&'//nompro//'.COORDO', 'V V R', nbnoeu*ndim, jcoord)
!
        if (ndim .eq. 2) then
            do 221 , iaux = 0 , nbnoeu-1
            zr(jcoord+2*iaux) = coordo(3*iaux+1)
            zr(jcoord+2*iaux+1) = coordo(3*iaux+2)
221          continue
        else
            do 222 , iaux = 0 , nbnoeu-1
            zr(jcoord+iaux) = coordo(3*iaux+1)
222          continue
        endif
!
        call mfcooe(idfimd, nomamd, zr(jcoord), edfuin, nbnoeu,&
                    codret)
!
        call jedetr('&&'//nompro//'.COORDO')
!
    endif
!
    if (codret .ne. 0) then
        saux08='MFCOOE  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!====
! 3. LES NOMS DES NOEUDS
!====
!
    call wkvect('&&'//nompro//'NOMNOE', 'V V K16', nbnoeu, adnomn)
!
    do 3 ino = 1, nbnoeu
        zk16(adnomn+ino-1) = nomnoe(ino)//'        '
!                                          12345678
 3  end do
!
    call mfnome(idfimd, nomamd, zk16(adnomn), nbnoeu, ednoeu,&
                tygeno, codret)
!
    if (codret .ne. 0) then
        saux08='MFNOME  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!====
! 4. LES RENUMEROTATIONS
!====
!
!    ON N'ECRIT PAS DE RENUMEROTATION CAR LES NOEUDS SONT NUMEROTES
!    DE 1 A NBNOEU, SANS TROU. DONC, INUTILE D'ENCOMBRER LE FICHIER.
!
!====
! 5. LA FIN
!====
!
    call jedetr('&&'//nompro//'NOMNOE')
!
    call jedema()
!
end subroutine
