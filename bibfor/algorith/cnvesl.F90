subroutine cnvesl(lischa, typres, neq, nompar, valpar,&
                  cnvass)
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
!
    implicit     none
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/fointc.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscpp.h"
#include "asterfort/lisico.h"
#include "asterfort/lislch.h"
#include "asterfort/lislco.h"
#include "asterfort/lislnf.h"
#include "asterfort/lisltc.h"
#include "asterfort/lisltf.h"
#include "asterfort/lisnbg.h"
#include "asterfort/lisnnb.h"
    character(len=19) :: lischa
    character(len=19) :: cnvass
    character(len=1) :: typres
    character(len=8) :: nompar
    integer :: neq
    real(kind=8) :: valpar, tval(1)
!
! ----------------------------------------------------------------------
!
! CALCUL CONTRIBUTION SECOND MEMBRE SI VECT_ASSE
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  TYPRES : TYPE DU CHAM_NO RESULTANT 'C'
! IN  NOMPAR : NOM DU PARAMETRE
! IN  VALPAR : VALEUR DU PARAMETRE
! IN  NEQ    : NOMBRE D'EQUATIONS DU SYSTEME
! OUT CNVASS : NOM DU CHAMP
!
! ----------------------------------------------------------------------
!
    integer :: ichar, nbchar
    integer :: nbveas, nbveag, nbtot, iret, ieq
    integer :: genrec
    integer ::  jvale
    character(len=16) :: typfct
    character(len=8) :: nomfct, charge, typech
    real(kind=8) :: valre, valim
    complex(kind=8) :: calpha, calp
    real(kind=8) :: phase, omega
    integer :: npuis
    logical :: lveas, lveag
    character(len=24) :: chamno
    complex(kind=8), pointer :: resu(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ASSERT(typres.eq.'C')
    omega = r8depi()*valpar
    call jeveuo(cnvass(1:19)//'.VALE', 'E', vc=resu)
    do 5 ieq = 1, neq
        resu(ieq) = dcmplx(0.d0,0.d0)
 5  continue
!
! --- NOMBRE DE CHARGEMENTS
!
    call lisnnb(lischa, nbchar)
!
! --- NOMBRE DE CHARGES DE TYPE VECT_ASSE
!
    nbveas = lisnbg(lischa,'VECT_ASSE' )
    nbveag = lisnbg(lischa,'VECT_ASSE_GENE')
    nbtot = nbveas+nbveag
    if (nbtot .eq. 0) goto 999
!
! --- BOUCLE SUR LES CHARGES
!
    do 10 ichar = 1, nbchar
!
! ----- CODE DU GENRE DE LA CHARGE
!
        call lislco(lischa, ichar, genrec)
!
! ----- FONCTION MULTIPLICATRICE
!
        call lislnf(lischa, ichar, nomfct)
        call lisltf(lischa, ichar, typfct)
!
! ----- MULTIPLICATEUR COMPLEXE
!
        call liscpp(lischa, ichar, phase, npuis)
!
        lveas = lisico('VECT_ASSE' ,genrec)
        lveag = lisico('VECT_ASSE_GENE',genrec)
        if (lveas .or. lveag) then
            call lislch(lischa, ichar, charge)
            call lisltc(lischa, ichar, typech)
            chamno = charge
!
            valre = 1.d0
            valim = 0.d0
            if (nomfct .ne. ' ') then
                tval(1)=valpar
                if (typfct(7:10) .eq. 'REEL') then
                    call fointe('F', nomfct, 1, nompar, tval,&
                                valre, iret)
                    valim = 0.d0
                else if (typfct(7:10).eq.'COMP') then
                    call fointc('F', nomfct, 1, nompar, tval,&
                                valre, valim, iret)
                else
                    ASSERT(.false.)
                endif
            endif
            calp = dcmplx(valre,valim)
            calpha = calp*exp(dcmplx(0.d0,phase*r8dgrd()))
            if (npuis .ne. 0) then
                calpha = calpha * omega**npuis
            endif
            call jeveuo(chamno(1:19)//'.VALE', 'L', jvale)
            if (typech .eq. 'COMP') then
                do 122 ieq = 1, neq
                    resu(ieq) = resu(ieq) + calpha*zc( jvale-1+ieq)
122              continue
            else
                do 123 ieq = 1, neq
                    resu(ieq) = resu(ieq) + calpha*zr( jvale-1+ieq)
123              continue
            endif
        endif
10  continue
!
999 continue
!
    call jedema()
end subroutine
