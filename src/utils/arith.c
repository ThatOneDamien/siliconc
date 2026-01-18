#include "lib.h"
#include "../core/internal.h"
#include <math.h>

#define HI32(_x) ((uint64_t)(_x) >> 32)
#define LO32(_x) ((_x) & 0xffffffff)
#define ISNEG(_x) ((uint64_t)(_x) >> 63)

// char *i128_to_string(Int128 op, uint64_t base, bool is_signed, bool use_prefix)
// {
// 	ASSERT(base >= 2 && base <= 16);
// 	static char digits[16] = "0123456789ABCDEF";
// 	char buffer[130];
// 	char *loc = buffer;
// 	bool add_minus = is_signed && ISNEG(op.hi);
// 	if (add_minus) op = i128_neg(op);
// 	Int128 base_div = { 0, base };
// 	do
// 	{
// 		Int128 rem = i128_urem(op, base_div);
// 		*(loc++) = digits[rem.lo];
// 		op = i128_udiv(op, base_div);
// 	} while (!i128_is_zero(op));
// 	char *res = malloc_string((size_t)(loc - buffer + 4));
// 	char *c = res;
// 	if (add_minus) *(c++) = '-';
// 	if (use_prefix)
// 	{
// 		switch (base)
// 		{
// 			case 2:
// 				*(c++) = '0';
// 				*(c++) = 'b';
// 				break;
// 			case 8:
// 				*(c++) = '0';
// 				*(c++) = 'o';
// 				break;
// 			case 16:
// 				*(c++) = '0';
// 				*(c++) = 'x';
// 				break;
// 			default:
// 				break;
// 		}
// 	}
// 	while (loc > buffer)
// 	{
// 		*(c++) = *(--loc);
// 	}
// 	*c = 0;
// 	return res;
// }

Int128 i128_add(Int128 lhs, Int128 rhs)
{
	uint64_t lo = lhs.lo + rhs.lo;
	uint64_t hi = lhs.hi + rhs.hi;
	if (lo < lhs.lo) hi++;
	return (Int128){ hi, lo };
}

Int128 i128_add64(Int128 lhs, uint64_t rhs)
{
	uint64_t lo = lhs.lo + rhs;
	return (Int128){ lo < lhs.lo ? lhs.hi + 1 : lhs.hi, lo };
}

Int128 i128_sub(Int128 lhs, Int128 rhs)
{
	uint64_t lo = lhs.lo - rhs.lo;
	uint64_t hi = lhs.hi - rhs.hi;

	if (lo > lhs.lo) hi--;
	return (Int128){ hi, lo };
}

Int128 i128_sub64(Int128 lhs, uint64_t rhs)
{
	uint64_t lo = lhs.lo - rhs;
	return (Int128){ lo > lhs.lo ? lhs.hi - 1 : lhs.hi, lo };
}

// Int128 i128_extend(Int128 op, TypeKind type)
// {
// 	int bits = type_kind_bitsize(type);
// 	if (bits == 128) return op;
// 	uint64_t shift = 128 - (uint64_t)bits;
// 	op = i128_shl64(op, shift);
// 	bool is_signed = type_kind_is_signed(type);
// 	if (is_signed)
// 	{
// 		return i128_ashr64(op, shift);
// 	}
// 	return i128_lshr64(op, shift);
// }

Int128 i128_and(Int128 lhs, Int128 rhs)
{
	return (Int128){ lhs.hi & rhs.hi, lhs.lo & rhs.lo };
}

Int128 i128_or(Int128 lhs, Int128 rhs)
{
	return (Int128){ lhs.hi | rhs.hi, lhs.lo | rhs.lo };
}

Int128 i128_xor(Int128 lhs, Int128 rhs)
{
	return (Int128){ lhs.hi ^ rhs.hi, lhs.lo ^ rhs.lo };
}

Int128 i128_neg(Int128 val)
{
	if (!val.lo && !val.hi) return val;
	return i128_add64(i128_not(val), 1);
}

Int128 i128_not(Int128 val)
{
	return (Int128){ ~val.hi, ~val.lo };
}

static Int128 int64_mult(uint64_t u, uint64_t v)
{
	uint64_t u1 = LO32(u);
	uint64_t v1 = LO32(v);
	uint64_t t = u1 * v1;
	uint64_t w3 = LO32(t);
	uint64_t k = HI32(t);

	u >>= 32;
	t = u * v1 + k;
	k = LO32(t);
	uint64_t w1 = HI32(t);

	v >>= 32;
	t = u1 * v + k;

	return (Int128){ (u * v) + w1 + HI32(t), (t << 32) + w3 };
}

Int128 i128_mult(Int128 lhs, Int128 rhs)
{
	Int128 lo_mult = int64_mult(lhs.lo, rhs.lo);
	lo_mult.hi += lhs.hi * rhs.lo + lhs.lo * rhs.hi;
	return lo_mult;
}

Int128 i128_mult64(Int128 lhs, uint64_t rhs)
{
	Int128 lo_mult = int64_mult(lhs.lo, rhs);
	lo_mult.hi += lhs.hi * rhs;
	return lo_mult;
}


int i128_ucmp(Int128 lhs, Int128 rhs)
{
	if (lhs.hi > rhs.hi) return 1;
	if (lhs.hi < rhs.hi) return -1;
	if (lhs.lo == rhs.lo) return 0;
	return lhs.lo > rhs.lo ? 1 : -1;
}

Int128 i128_shl64(Int128 lhs, uint64_t amount)
{
	if (amount == 0) return lhs;
	if (amount > 127) return (Int128){ 0, 0 };
	if (amount == 64) return (Int128){ lhs.lo, 0 };
	if (amount > 64) return (Int128){ lhs.lo << (amount - 64), 0 };
	return (Int128){ (lhs.hi << amount) | lhs.lo >> (64 - amount), lhs.lo << amount };
}

Int128 i128_shl(Int128 lhs, Int128 op)
{
	if (op.hi) return (Int128){ 0, 0 };
	return i128_shl64(lhs, op.lo);
}

Int128 i128_lshr64(Int128 lhs, uint64_t amount)
{
	if (amount == 0) return lhs;
	if (amount > 127) return (Int128){ 0, 0 };
	if (amount == 64) return (Int128){ 0, lhs.hi };
	if (amount > 64) return (Int128){ 0, lhs.hi >> (amount - 64) };
	lhs.lo >>= amount;
	lhs.lo |= lhs.hi << (64 - amount);
	lhs.hi >>= amount;
	return lhs;
}

bool i128_is_zero(Int128 op)
{
	return op.hi == 0 && op.lo == 0;
}

Int128 i128_lshr(Int128 lhs, Int128 rhs)
{
	if (rhs.hi != 0) return (Int128){ 0, 0 };
	return i128_lshr64(lhs, rhs.lo);
}

Int128 i128_ashr64(Int128 lhs, uint64_t amount)
{
	if (amount == 0) return lhs;
	if (!ISNEG(lhs.hi)) return i128_lshr64(lhs, amount);
	if (amount > 127) return (Int128){ UINT64_MAX, UINT64_MAX };
	if (amount == 64) return (Int128){ UINT64_MAX, lhs.hi };
	if (amount > 64) return (Int128){ UINT64_MAX, (uint64_t)(((int64_t)lhs.hi) >> (amount - 64)) };
	return (Int128){ (uint64_t)(((int64_t)lhs.hi) >> amount), lhs.lo >> amount | (lhs.hi << (64 - amount)) };
}

Int128 i128_ashr(Int128 lhs, Int128 rhs)
{
	if (rhs.hi != 0) return ISNEG(lhs.hi) ? (Int128){ UINT64_MAX, UINT64_MAX } : (Int128){ 0, 0 };
	return i128_ashr64(lhs, rhs.lo);
}

bool i128_is_neg(Int128 op)
{
	return ISNEG(op.hi);
}

int i128_cmp(Int128 lhs, Int128 rhs, TypeKind kind)
{
	return type_kind_is_signed(kind) ? i128_scmp(lhs, rhs) : i128_ucmp(lhs, rhs);
}

int i128_scmp(Int128 lhs, Int128 rhs)
{
	bool lhs_sign = lhs.hi & ((uint64_t)INT64_MIN);
	bool rhs_sign = rhs.hi & ((uint64_t)INT64_MIN);
	if (lhs_sign != rhs_sign)
	{
		return lhs_sign ? -1 : 1;
	}
	if (lhs.hi > rhs.hi) return 1;
	if (lhs.hi < rhs.hi) return -1;
	if (lhs.lo == rhs.lo) return 0;
	return lhs.lo > rhs.lo ? 1 : -1;
}

static uint32_t popcnt64(uint64_t n)
{
	n -= ((n >> 1) & 0x5555555555555555);
	n = (n & 0x3333333333333333) + ((n >> 2) & 0x3333333333333333);
	return (((n + (n >> 4)) & 0xF0F0F0F0F0F0F0F) * 0x101010101010101) >> 56;
}

UNUSED uint32_t i128_popcnt(Int128 i)
{
	return popcnt64(i.hi) + popcnt64(i.lo);
}

static uint32_t ctz64(uint64_t n)
{
	uint64_t i = ~n;
	uint32_t c = ((i ^ (i + 1)) & i) >> 63;

	i = LO32(n) + 0xffffffff;
	i = ((i & 0x100000000) ^ 0x100000000) >> 27;
	c += i;
	n >>= i;

	i = (n & 0xffff) + 0xffff;
	i = ((i & 0x10000) ^ 0x10000) >> 12;
	c += i;
	n >>= i;

	i = (n & 0xff) + 0xff;
	i = ((i & 0x100) ^ 0x100) >> 5;
	c += i;
	n >>= i;

	i = (n & 0xf) + 0xf;
	i = ((i & 0x10) ^ 0x10) >> 2;
	c += i;
	n >>= i;

	i = (n & 3) + 3;
	i = ((i & 4) ^ 4) >> 1;
	c += i;
	n >>= i;

	c += ((n & 1) ^ 1);
	return c;
}

uint32_t i128_ctz(const Int128 *op)
{
	return !op->lo ? ctz64(op->hi) + 64 : ctz64(op->lo);
}

static uint32_t clz64(uint64_t n)
{
	uint64_t neg_n = ~n;
	uint32_t c = ((neg_n ^ (neg_n + 1)) & neg_n) >> 63;

	neg_n = (n >> 32) + 0xffffffff;
	neg_n = ((neg_n & 0x100000000) ^ 0x100000000) >> 27;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 48) + 0xffff;
	neg_n = ((neg_n & 0x10000) ^ 0x10000) >> 12;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 56) + 0xff;
	neg_n = ((neg_n & 0x100) ^ 0x100) >> 5;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 60) + 0xf;
	neg_n = ((neg_n & 0x10) ^ 0x10) >> 2;
	c += neg_n;
	n <<= neg_n;

	neg_n = (n >> 62) + 3;
	neg_n = ((neg_n & 4) ^ 4) >> 1;
	c += neg_n;
	n <<= neg_n;

	c += (n >> 63) ^ 1;

	return c;
}

uint32_t i128_clz(const Int128 *op)
{
	return op->hi ? clz64(op->hi) : clz64(op->lo) + 64;
}

double i128_to_float(Int128 op, TypeKind kind)
{
	return type_kind_is_signed(kind) ? i128_to_float_signed(op) : i128_to_float_unsigned(op);
}

double i128_to_float_signed(Int128 op)
{
	return (double)op.lo + ldexp((double)((int64_t)op.hi), 64);
}

double i128_to_float_unsigned(Int128 op)
{
	return (double)op.lo + ldexp((double)op.hi, 64);
}

void i128_udivrem(Int128 lhs, Int128 rhs, Int128 *div, Int128 *rem)
{
	*div = (Int128){ 0, 0 };
	int32_t shift = (int32_t)(i128_clz(&rhs) - i128_clz(&lhs));
	if (shift < 0)
	{
		*rem = lhs;
		return;
	}
	rhs = i128_shl64(rhs, (uint64_t)shift);
	do
	{
		*div = i128_shl64(*div, 1);
		if (i128_ucmp(lhs, rhs) != -1)
		{
			lhs = i128_sub(lhs, rhs);
			div->lo |= 1;
		}
		rhs = i128_lshr64(rhs, 1);

	} while (shift-- != 0);
	rem->hi = lhs.hi;
	rem->lo = lhs.lo;
}

Int128 i128_udiv(Int128 lhs, Int128 rhs)
{
	Int128 div, rem;
	i128_udivrem(lhs, rhs, &div, &rem);
	return div;
}

Int128 i128_urem(Int128 lhs, Int128 rhs)
{
	Int128 div, rem;
	i128_udivrem(lhs, rhs, &div, &rem);
	return rem;
}

Int128 i128_srem(Int128 lhs, Int128 rhs)
{
	uint64_t topbit1 = lhs.hi & 0x8000000000000000;
	uint64_t topbit2 = rhs.hi & 0x8000000000000000;
	if (topbit1) lhs = i128_neg(lhs);
	if (topbit2) rhs = i128_neg(rhs);
	Int128 res = i128_urem(lhs, rhs);
	if (topbit2 ^ topbit1)
	{
		return i128_neg(res);
	}
	return res;
}

Int128 i128_from_signed(int64_t i)
{
	return (Int128){ i < 0 ? UINT64_MAX : 0, (uint64_t)i };
}

UNUSED Int128 i128_from_unsigned(uint64_t i)
{
	return (Int128){ 0, i };
}

Int128 i128_sdiv(Int128 lhs, Int128 rhs)
{
	uint64_t topbit1 = lhs.hi & 0x8000000000000000;
	uint64_t topbit2 = rhs.hi & 0x8000000000000000;
	if (topbit1) lhs = i128_neg(lhs);
	if (topbit2) rhs = i128_neg(rhs);
	Int128 res = i128_udiv(lhs, rhs);
	if (topbit2 ^ topbit1)
	{
		return i128_neg(res);
	}
	return res;
}

bool int128_fits(Int128 val, TypeKind optype, TypeKind totype)
{
	Int128 min;
	Int128 max;
	bool is_signed = false;
	switch (totype)
	{
    case TYPE_BYTE:
        min = (Int128){ UINT64_MAX, INT8_MIN };
        max = (Int128){ 0, INT8_MAX };
        is_signed = true;
        break;
    case TYPE_UBYTE:
        max = (Int128){ 0, UINT8_MAX };
        break;
    case TYPE_SHORT:
        min = (Int128){ UINT64_MAX, INT16_MIN };
        max = (Int128){ 0, INT16_MAX };
        is_signed = true;
        break;
    case TYPE_USHORT:
        max = (Int128){ 0, UINT16_MAX };
        break;
    case TYPE_INT:
        min = (Int128){ UINT64_MAX, INT32_MIN };
        max = (Int128){ 0, INT32_MAX };
        is_signed = true;
        break;
    case TYPE_UINT:
        max = (Int128){ 0, UINT32_MAX };
        break;
    case TYPE_LONG:
        min = (Int128){ UINT64_MAX, INT64_MIN };
        max = (Int128){ 0, INT64_MAX };
        is_signed = true;
        break;
    case TYPE_ULONG:
        max = (Int128){ 0, UINT64_MAX };
        break;
    case TYPE_INT128:
        min = INT128_MIN;
        max = INT128_MAX;
        is_signed = true;
        break;
    case TYPE_UINT128:
        max = UINT128_MAX;
        break;
    default:
        SIC_UNREACHABLE();
	}
	bool op_is_signed = type_kind_is_signed(optype);
	if (is_signed)
	{
		if (op_is_signed)
		{
			if (i128_scmp(val, min) == -1) return false;
			if (i128_scmp(val, max) == 1) return false;
			return true;
		}
		// In the unsigned case, we don't need to test the loer limit.
		return i128_ucmp(val, max) != 1;
	}
	if (op_is_signed)
	{
		if (i128_is_neg(val)) return false;
		if (i128_ucmp(val, max) == 1) return false;
		return true;
	}
	// In the unsigned case, we don't need to test the loer limit.
	return i128_ucmp(val, max) != 1;
}

Int128 i128_from_double(double x, TypeKind kind)
{
	return type_kind_is_signed(kind) ? i128_signed_from_double(x) : i128_unsigned_from_double(x);
}

Int128 i128_unsigned_from_double(double x)
{
	if (x >= ldexp(1, 64))
	{
		uint64_t hi = (uint64_t)ldexp(x, -64);
		uint64_t lo = (uint64_t)(x - ldexp((double)hi, 64));
		return (Int128){ hi, lo };
	}
	return (Int128){ 0, (uint64_t)x };
}

Int128 i128_signed_from_double(double x)
{
	return x < 0 ? i128_neg(i128_unsigned_from_double(-x)) : i128_signed_from_double(x);
}
