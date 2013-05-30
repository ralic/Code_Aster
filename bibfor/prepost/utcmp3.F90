subroutine utcmp3(nbcmp, nomcmp, numcmp)
    implicit   none
    include 'asterfort/lxliis.h'
    include 'asterfort/u2mesg.h'
    integer :: nbcmp, numcmp(*)
    character(len=*) :: nomcmp(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! BUT :  VERIFIER QUE LES COMPOSANTES DE LA GRANDEUR VARI_R S'APPELLENT
!        BIEN V1, V2, .... ET RETOURNER LA LISTE DES NUMEROS CONCERNES
!
! ARGUMENTS :
!  IN NBCMP      : NOMBRE DE CMPS A VERIFIER
!  IN NOMCMP(*)  : LISTE DES NOMS DES CMPS    : (V1, V4, ...)
!  OUT NUMCMP(*) : LISTE DES NUMEROS DES CMPS : ( 1,  4, ...)
!
!
! ----------------------------------------------------------------------
    integer :: i, iret
    character(len=8) :: nom
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
    do 10 i = 1, nbcmp
        nom = nomcmp(i)
        numcmp(i) = 0
!
        call lxliis(nom(2:8), numcmp(i), iret)
!
        if (iret .ne. 0) then
            valk (1) = nom
            valk (2) = 'VARI_R'
            call u2mesg('F', 'CALCULEL6_49', 2, valk, 0,&
                        0, 0, 0.d0)
        endif
10  end do
!
end subroutine
