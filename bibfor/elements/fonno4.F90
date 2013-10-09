subroutine fonno4(ndim, macofo, noma, nbmac, tablev,&
                  noe, nbnoff, indic)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!
    character(len=8) :: noma
    character(len=19) :: macofo
    integer :: ndim, nbmac, tablev(2), noe(4, 4), nbnoff, indic(4)
!
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
!       ----------------------------------------------------------------
!       FILTRE DES FACES LIBRES
!       ----------------------------------------------------
!    ENTREES
!       NDIM   : DIMENSION DU MAILLAGE
!       MACOFO : VECTEUR DES MAILLES (PRINCIPALES) CONNECTEES AU SEGMENT
!                DU FOND DE FISSURE COURANT
!       NOMA   : NOM DU MAILLAGE
!       NBMAC : NOMBRE DE MAILLES CONNECTEES AU SEGMENT DU FOND ET DE
!               DE DIMENSION NDIM
!       TABLEV : VECTEUR CONTNANT LES NUMEROS DES DEUX MAILLES
!                CONNECTEES AU NOEUD SOMMET COURANT ET AUX LEVRES
!       NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
!                MAILLES CONNECTEES AU NOEUD SOMMET COURANT
!                ET AUX LEVRES
!       NBNOFF : NOMBRE DE NOEUD EN FOND DE FISSURE
!    SORTIE
!       INDIC  : INDICE DES FACES / ARETES INTERNES
!
!       ----------------------------------------------------
!
    integer :: jmaco, iatyma, iamase, ityp
    integer :: comp5, ima, inp, inq, compte, nn, i, j, ino1
    character(len=8) :: type
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!
!     RECUPERATION DE L'ADRESSE DES TYPFON DE MAILLES
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
!     RECUPERATION DU VECTEUR DES MAILLES CONNECTEES AU SEGMENT DU FOND
    call jeveuo(macofo, 'L', jmaco)
!
    indic(1)=0
    indic(2)=0
    indic(3)=0
    indic(4)=0
    comp5=0
!     ON BALAYE LES MAILLES CONNECTEES AU NOEUD INO
    do ima = 1, nbmac
!       POUR CHAQUE FACE RETENUE
        do inp = 1, 4
            compte=0
!         ON NE CONSIDERE QUE LES MAILLES INTERNES AFIN D'ELIMINER
!         LES FACES (EN 3D) OU LES SEGMENTS INTERNES (EN 2D)
            if ((zi(jmaco-1 + ima).ne.tablev(1)) .and. (zi(jmaco-1 + ima).ne.tablev(2))) then
                ityp = iatyma-1+zi(jmaco-1 + ima)
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
                call dismoi('NBNO_TYPMAIL', type, 'TYPE_MAILLE', repi=nn)
                call jeveuo(jexnum( noma//'.CONNEX', zi(jmaco-1 + ima) ), 'L', iamase)
!           POUR CHAQUE NOEUD DE LA MAILLE INTERNE
                do i = 1, nn
!             ON COMPTE LE NOMBRE DE NOEUDS COMMUN AVEC LA FACE INP
                    do ino1 = 1, 4
                        if (noe(inp,ino1) .ne. 0) then
                            if (zi(iamase-1+i) .eq. noe(inp,ino1)) then
                                compte = compte+1
                            endif
                        endif
                    end do
                end do
            endif
!         LES FACES A NE PAS PRENDRE EN COMPTE CAR INTERNE
            if (((nbnoff.gt.1).and.(compte.ge.3)) .or. ((nbnoff.eq.1) .and.(compte.ge.2))) then
                comp5 = comp5 + 1
                indic(comp5) = inp
            endif
        end do
    end do
!
!     CAS PARTICULIER OU AUCUNE MAILLE INTERNE N'EST PRESENTE
    if ((comp5.eq.0) .and. (nbmac.eq.2)) then
        do inp = 1, 4
            do inq = 1, 4
                compte = 0
                if (inp .ne. inq) then
                    do i = 1, 4
                        do j = 1, 4
                            if (noe(inp,i) .ne. 0) then
                                if (noe(inp,i) .eq. noe(inq,j)) then
                                    compte = compte+1
                                endif
                            endif
                        end do
                    end do
                endif
                if (ndim .eq. 3 .and. compte .ge. 3 .or. ndim .eq. 2 .and. compte .ge. 2) then
                    indic(inp)=inq
                endif
            end do
        end do
    endif
!
!
    call jedema()
end subroutine
