subroutine cfgcrl(resoco, neq, nbliai, matass, solveu,&
                  alpha)
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
    implicit none
#include "jeveux.h"
#include "asterfort/calatm.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
    character(len=24) :: resoco
    integer :: neq, nbliai
    character(len=19) :: matass, solveu
    real(kind=8) :: alpha
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - GCP)
!
! RECHERCHE LINEAIRE
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! OUT ALPHA  : COEFFICIENT DE RECHERCHE LINEAIRE
!
!
!
!
    integer :: ifm, niv
    real(kind=8) :: numer, denom
    integer :: iliai, jdecal, nbddl
    complex(kind=8) :: c16bid
    character(len=19) :: k19bla
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=19) :: sgradp, sgrprp, direct
    integer :: jsgrap, jsgprp, jdirec
    character(len=24) :: secmbr, ddelt, cncin0
    integer :: jsecmb, jddelt
    integer :: iret
    c16bid = dcmplx(0.d0, 0.d0)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    k19bla = ' '
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    direct = resoco(1:14)//'.DIRE'
    sgradp = resoco(1:14)//'.SGDP'
    sgrprp = resoco(1:14)//'.SGPP'
!
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(direct, 'L', jdirec)
    call jeveuo(sgradp, 'L', jsgrap)
    call jeveuo(sgrprp, 'L', jsgprp)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
!
    secmbr = resoco(1:14)//'.SECM'
    cncin0 = resoco(1:14)//'.CIN0'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddelt(1:19) //'.VALE', 'E', jddelt)
    call jeveuo(secmbr(1:19)//'.VALE', 'E', jsecmb)
!
! --- INITIALISATIONS DES VECTEURS DE TRAVAIL
!
    call r8inir(neq, 0.d0, zr(jsecmb), 1)
    call r8inir(neq, 0.d0, zr(jddelt), 1)
!
! --- SECOND MEMBRE: [A]T .{DIRECP}
!
    do iliai = 1, nbliai
        jdecal = zi(japptr+iliai-1)
        nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
        call calatm(neq, nbddl, zr(jdirec+iliai-1), zr(japcoe+jdecal), zi(japddl+jdecal),&
                    zr(jsecmb))
    end do
!
! --- RESOLUTION [K].{DDELT} = [A]T .{DIRECP} -> {DDELT}
!
    call resoud(matass, k19bla, solveu, cncin0, 0,&
                secmbr, ddelt, 'V', [0.d0], [c16bid],&
                k19bla, .true., 0, iret)
!
! --- PRODUIT SCALAIRE  NUMER = <DIRECP>.{DIRECP}
!
    numer = ddot(nbliai,zr(jsgprp),1,zr(jsgrap),1)
!
! --- PRODUIT SCALAIRE  DENOM = <DIRECP>.[A].[K]-1.[A]T .{DIRECP}
!
    call jeveuo(ddelt(1:19) //'.VALE', 'L', jddelt)
    denom = ddot(neq,zr(jddelt),1,zr(jsecmb),1)
!
    if (denom .lt. 0.d0) then
        call utmess('A', 'CONTACT_7')
    endif
!
! --- COEFFICIENT DE RECHERCHE LINEAIRE
!
    alpha = numer/denom
!
! --- AFFICHAGE
!
    if (niv .eq. 2) then
        write (ifm,9040) alpha
    endif
!
    9040 format (' <CONTACT><CALC> PAS D''AVANCEMENT INITIAL : ',1pe12.5)
!
    call jedema()
!
end subroutine
