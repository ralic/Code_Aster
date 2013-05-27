subroutine fovern(vecnom, nbfonc, vecpro, ier)
    implicit none
    include 'jeveux.h'
    include 'asterfort/fonbpa.h'
    include 'asterfort/fopro1.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesg.h'
    integer :: nbfonc, ier
    character(len=*) :: vecnom(nbfonc), vecpro(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     VERIFICATION DE L'HOMOGENEITE DES PARAMETRES DES FONCTIONS
!     COMPOSANT UNE NAPPE
!     STOCKAGE DE CE PARAMETRE UNIQUE ET DES TYPES DE PROLONGEMENTS
!     ET D'INTERPOLATION DE CHAQUE FONCTION
!     ------------------------------------------------------------------
! IN  VECNOM: VECTEUR DES NOMS DES FONCTIONS
! IN  NBFONC: NOMBRE DE FONCTIONS
! OUT    VECPRO: VECTEUR DESCRIPTEUR DE LA NAPPE
!     ------------------------------------------------------------------
!     OBJETS SIMPLES LUS
!        CHNOM=VECNOM(I)//'.PROL'
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: i, jprof, nbpf
    integer :: vali
    character(len=24) :: chnom
    character(len=24) :: valk(3)
    character(len=16) :: prolgd, interp, typfon, nompf(10)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    chnom(20:24) = '.PROL'
    do 1 i = 1, nbfonc
        chnom(1:19) = vecnom(i)
        call jeveuo(chnom, 'L', jprof)
        call fopro1(zk24(jprof), 0, prolgd, interp)
        call fonbpa(chnom(1:19), zk24(jprof), typfon, 10, nbpf,&
                    nompf)
        call jelibe(chnom)
        if (nompf(1) .ne. 'TOUTPARA') then
            vecpro(7)=nompf(1)
            goto 2
        endif
 1  end do
    vali = nbfonc
    call u2mesg('E', 'UTILITAI8_1', 0, ' ', 1,&
                vali, 0, 0.d0)
    ier=ier+1
 2  continue
    do 3 i = 1, nbfonc
        chnom(1:19) = vecnom(i)
        call jeveuo(chnom, 'L', jprof)
        call fopro1(zk24(jprof), 0, prolgd, interp)
        call fonbpa(chnom(1:19), zk24(jprof), typfon, 10, nbpf,&
                    nompf)
        call jelibe(chnom)
        if (nompf(1) .ne. vecpro(7) .and. nompf(1) .ne. 'TOUTPARA') then
            valk (1) = vecnom(i)
            valk (2) = nompf(1)
            valk (3) = vecpro(7)
            call u2mesg('E', 'UTILITAI8_2', 3, valk, 0,&
                        0, 0, 0.d0)
            ier=ier+1
        endif
        vecpro(7+2*i-1) = interp
        vecpro(7+2*i ) = prolgd
 3  end do
    call jedema()
end subroutine
