subroutine nmresg(numedd, sddyna, instap, cndonn, accsol)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdgeph.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtzero.h"
#include "blas/ddot.h"
    real(kind=8) :: instap
    character(len=19) :: cndonn, sddyna
    character(len=24) :: numedd
    character(len=19) :: accsol
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! RESOLUTION SUR DDLS GENERALISES
!
! ----------------------------------------------------------------------
!
!
! IN  INSTAP : INSTANT COURANT
! IN  NUMEDD : NUME_DDL
! IN  CNDONN : CHAM_NO POUR LE SECOND MEMBRE
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! I/O ACCSOL : ACCELERATION CALCULEE
!
!
!
!
    integer ::  ier
    integer :: ifonc, imode, imode2
    integer :: neq, nbgene, nbmodp
    integer :: j2memb, jaccp, jaccg
    logical :: lexge, lacce
    character(len=19) :: fmodal, valfon
    integer :: jfmoda, jvalfo
    character(len=19) :: depgep, vitgep, accgep
    integer :: jdepgp, jvitgp, jaccgp
    character(len=19) :: basmod, masgen, amogen, riggen
    integer :: jbasmo, jmasge, jamoge, jrigge
    character(len=19) :: fongen, forgen
    integer :: jfonge, jforge
    character(len=19) :: accgcn
    integer :: jacccn
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> RESOLUTION SUR BASE MODALE'
    endif
!
! --- INITIALISATIONS
!
    call vtzero(accsol)
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- FONCTIONNALITES ACTIVEES
!
    lexge = ndynlo(sddyna,'EXPL_GENE')
    lacce = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.3
    if (.not.lacce) then
        ASSERT(.false.)
    endif
!
! --- OBJETS PROJECTION MODALE
!
    call ndynkk(sddyna, 'PRMO_DEPGEP', depgep)
    call ndynkk(sddyna, 'PRMO_VITGEP', vitgep)
    call ndynkk(sddyna, 'PRMO_ACCGEP', accgep)
    call ndynkk(sddyna, 'PRMO_BASMOD', basmod)
    call ndynkk(sddyna, 'PRMO_MASGEN', masgen)
    call ndynkk(sddyna, 'PRMO_AMOGEN', amogen)
    call ndynkk(sddyna, 'PRMO_RIGGEN', riggen)
    call ndynkk(sddyna, 'PRMO_FONGEN', fongen)
    call ndynkk(sddyna, 'PRMO_FORGEN', forgen)
    call ndynkk(sddyna, 'PRMO_ACCGCN', accgcn)
    call ndynkk(sddyna, 'PRMO_VALFON', valfon)
    call ndynkk(sddyna, 'PRMO_FMODAL', fmodal)
    call jeveuo(masgen, 'L', jmasge)
    call jeveuo(basmod, 'L', jbasmo)
    call jeveuo(fmodal, 'E', jfmoda)
!
! --- NOMBRE DE MODES
!
    nbmodp = ndynin(sddyna,'NBRE_MODE_PROJ')
!
! --- VECTEUR SECOND MEMBRE
!
    call jeveuo(cndonn(1:19)//'.VALE', 'E', j2memb)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> -> SECOND MEMBRE DONNE'
        call nmdebg('VECT', cndonn, 6)
    endif
!
! --- RECUPERATION VECTEUR DES FORCES GENERALISEES
!
    if (lexge) then
        call jeveuo(riggen, 'L', jrigge)
        call jeveuo(amogen, 'L', jamoge)
        call jeveuo(vitgep, 'L', jvitgp)
        call jeveuo(depgep, 'L', jdepgp)
        call jeveuo(accgep, 'E', jaccgp)
!
! --- FORCES GENERALISEES ?
!
        nbgene = ndynin(sddyna,'NBRE_EXCIT_GENE')
!
! --- EVALUATION DES FONCTIONS MULTIPLICATRICES
!
        if (nbgene .gt. 0) then
            call jeveuo(fongen, 'L', jfonge)
            call jeveuo(forgen, 'L', jforge)
            call jeveuo(valfon, 'E', jvalfo)
            do 14 ifonc = 1, nbgene
                call fointe('F ', zk24(jfonge+ifonc-1)(1:8), 1, ['INST'], [instap],&
                            zr(jvalfo+ifonc-1), ier)
 14         continue
        endif
!
! --- CALCUL DES FORCES MODALES
!
        do 11 imode = 1, nbmodp
            zr(jfmoda+imode-1) = ddot( neq,zr(jbasmo+(imode-1)*neq),1, zr(j2memb),1)
            do 12 imode2 = 1, nbmodp
                zr(jfmoda+imode-1) = zr(jfmoda+imode-1) - zr(jrigge+( imode2-1)*nbmodp+imode-1)*z&
                                     &r(jdepgp+imode2-1) - zr( jamoge+(imode2-1)*nbmodp+imode-1)*&
                                     &zr(jvitgp+imode2-1)
 12         continue
            do 15 ifonc = 1, nbgene
                zr(jfmoda+imode-1) = zr(jfmoda+imode-1) + zr(jforge+( ifonc-1)*nbmodp+imode-1)*zr&
                                     &(jvalfo+ifonc-1)
 15         continue
 11     continue
!
! --- CALCUL DES ACCELERATIONS GENERALISEES
!
        do 71 imode = 1, nbmodp
            zr(jaccgp+imode-1) = zr(jfmoda+imode-1)/zr(jmasge+imode-1)
 71     continue
!
        jaccg = jaccgp
!
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE><RESO> -> SOLUTION (MODALE):'
            call nmdebg(' ', accgep, 6)
        endif
    else
        call jeveuo(accgcn, 'E', jacccn)
!
! --- CALCUL DES FORCES GENERALISEES
!
        do 13 imode = 1, nbmodp
            zr(jfmoda+imode-1) = ddot( neq,zr(jbasmo+(imode-1)*neq),1, zr(j2memb),1)
 13     continue
!
! --- CALCUL DES ACCELERATIONS GENERALISEES
!
        do 74 imode = 1, nbmodp
            zr(jacccn+imode-1) = zr(jfmoda+imode-1)/zr(jmasge+imode-1)
 74     continue
!
        jaccg = jacccn
!
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE><RESO> -> SOLUTION (MODALE):'
            call nmdebg(' ', accgcn, 6)
        endif
    endif
!
! --- CALCUL DES ACCELERATIONS PHYSIQUES
!
    call jeveuo(accsol(1:19)//'.VALE', 'E', jaccp)
    call mdgeph(neq, nbmodp, zr(jbasmo), zr(jaccg), zr(jaccp))
!
! --- AFFICHAGE DES SOLUTIONS
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><RESO> -> SOLUTION (PHYSIQUE):'
        call nmdebg('VECT', accsol, 6)
    endif
!
    call jedema()
end subroutine
