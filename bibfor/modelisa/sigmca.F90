subroutine sigmca(tablca, carsig, icabl, nbnoca, numaca,&
                  quad)
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!  DESCRIPTION : MISE A JOUR DE LA CARTE ELEMENTAIRE DES CONTRAINTES
!  -----------   INITIALES POUR LE CABLE COURANT
!                APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!  IN     : TABLCA : CHARACTER*19
!                    NOM DE LA TABLE DECRIVANT LES CABLES
!  IN     : CARSIG : CHARACTER*19 , SCALAIRE
!                    NOM DE LA CARTE ELEMENTAIRE DES CONTRAINTES
!                    INITIALES
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  IN     : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
!                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
!  IN     : NUMACA : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES MAILLES APPARTENANT AUX CABLES
!  IN     : QUAD   : VRAI SI MAILLAGE QUADRATIQUE (SEG3)
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
    logical :: quad
    character(len=19) :: carsig, numaca, tablca
    integer :: icabl, nbnoca(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: idecma, idecno, imail, ipara, jnumac, jtblp, jtbnp, jtens, jvalv, nblign, nbmaca
    integer :: nbno, nbpara, numail, nbma, mma
    character(len=1) :: k1b
    character(len=24) :: tens
    real(kind=8) :: rtens
    logical :: trouve
!
    character(len=24) :: parcr
    data          parcr /'TENSION                 '/
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   ACCES AUX DONNEES ET AUX RESULTATS DE CALCUL UTILES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    nbno = nbnoca(icabl)
!
! 1.1 RECUPERATION DE LA TENSION LE LONG DES CABLES
! ---
    call jeveuo(tablca//'.TBNP', 'L', jtbnp)
    nbpara = zi(jtbnp)
    nblign = zi(jtbnp+1)
    call jeveuo(tablca//'.TBLP', 'L', jtblp)
    trouve = .false.
    do ipara = 1, nbpara
        if (zk24(jtblp+4*(ipara-1)) .eq. parcr) then
            trouve = .true.
            tens = zk24(jtblp+4*(ipara-1)+2)
            call jeveuo(tens, 'L', jtens)
        endif
        if (trouve) goto 11
    end do
11  continue
    idecno = nblign - nbno
!
! 1.2 NUMEROS DES MAILLES APPARTENANT AUX CABLES
! ---
    call jelira(numaca, 'LONUTI', nbmaca)
    call jeveuo(numaca, 'L', jnumac)
    if (quad) then
        ASSERT((mod(nbno-1, 2).eq.0))
        nbma=(nbno-1)/2
        mma = 2
    else
        nbma = nbno-1
        mma = 1
    endif
    idecma = nbmaca - nbma
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   MISE A JOUR DE LA CARTE DES CONTRAINTES INITIALES AUX ELEMENTS
!     DES CABLES
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!.... BOUCLE SUR LE NOMBRE DE MAILLES DU CABLE COURANT
!
    call jeveuo(carsig//'.VALV', 'E', jvalv)
    do imail = 1, nbma
        numail = zi(jnumac+idecma+imail-1)
        rtens = ( zr(jtens+idecno+mma*imail-mma) + zr(jtens+idecno+ mma*imail) ) / 2.0d0
        zr(jvalv)=rtens
        zr(jvalv+1)=0.d0
        zr(jvalv+2)=0.d0
        call nocart(carsig, 3, k1b, 'NUM', 1,&
                    k1b, numail, ' ', 3)
    end do
!
    call jedema()
!
! --- FIN DE SIGMCA.
end subroutine
