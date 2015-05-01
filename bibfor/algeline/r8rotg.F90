subroutine r8rotg(da, db, dc, ds)
    implicit none
    real(kind=8) :: da, db, dc, ds
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!        ROTATON PLANE  (METHODE DE GIVENS).
!-----------------------------------------------------------------------
! I/O : DA   : PREMIER ELEMENT DU VECTEUR.
!         OUT: R = (+/-)SQRT(DA**2 + DB**2) OVERWRITES DA.
!     : DB   : DEUXIEME ELEMENT DU VECTEUR.
!         OUT: ZDB OU Z EST DEFINI PAR
!                 DS        SI ABS(DA) .GT. ABS(DB)
!                 1.0D0/DC  SI ABS(DB) .GE. ABS(DA) ET DC .NE. 0.0D0
!                 1.0D0     SI DC .EQ. 0.0D0.
! OUT : DC   : COEFFICIENT DE ROTATION.
!     : DC   - COEFFICIENT DE ROTATION.
!-----------------------------------------------------------------------
    real(kind=8) :: r, u, v
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (abs(da) .gt. abs(db)) then
!                                       ABS(DA) .GT. ABS(DB)
        u = da + da
        v = db/u
!                     U ET R MEME SIGNE QUE DA
        r = sqrt(.25d0+v**2)*u
!                     DC EST POSITIF
        dc = da/r
        ds = v*(dc+dc)
        db = ds
        da = r
!                                   ABS(DA) .LE. ABS(DB)
    else
        if (db .ne. 0.0d0) then
            u = db + db
            v = da/u
!
!                   U ET R ONT MEME SIGNE QUE
!            DB (R EST IMMEDIATEMENT STOCKE DANS DA)
            da = sqrt(.25d0+v**2)*u
!                                  DS EST POSITIF
            ds = db/da
            dc = v*(ds+ds)
            if (dc .ne. 0.0d0) then
                db = 1.0d0/dc
            else
                db = 1.0d0
            endif
        else
!                                   DA = DB = 0.D0
            dc = 1.0d0
            ds = 0.0d0
            da = 0.0d0
            db = 0.0d0
        endif
    endif
end subroutine
